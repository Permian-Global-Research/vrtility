#' format a date for a STAC query
#' @param x A character string of a date
#' @return A character string of the date in the correct format
#' @noRd
#' @keywords internal
format_stac_date <- function(x) {
  format(lubridate::as_datetime(x), "%Y-%m-%dT%H:%M:%SZ")
}


#' make a request to a stac source using GET first then POST if it fails.
#' @param search_obj An rstac query object
#' @return an rstac_doc object
#' @noRd
#' @keywords internal
execute_stac_request <- function(search_obj) {
  # Try GET first, fall back to POST if needed
  tryCatch(
    {
      rstac::get_request(search_obj)
    },
    error = function(e) {
      cli::cli_inform("GET request failed, attempting to POST request")
      rstac::post_request(search_obj)
    }
  )
}


#' Get all collections from a STAC API with pagination support using httr2 only
#' @param stac_source A STAC source URL (character string)
#' @param target_collection Optional collection ID to search for - exits early if found
#' @param id_only Logical, if TRUE (default) return only collection IDs, if FALSE return full collection objects
#' @return A character vector of collection IDs (if id_only=TRUE) or a list of all collections (if id_only=FALSE)
#' @noRd
#' @keywords internal
get_all_collections <- function(
  stac_source,
  target_collection = NULL,
  id_only = TRUE
) {
  all_collections <- list()

  # Start with first page - construct collections URL
  collections_url <- paste0(stac_source, "/collections")

  repeat {
    # Fetch collections page using httr2
    collections_response <- try(
      {
        httr2::request(collections_url) |>
          httr2::req_url_query(limit = 100) |>
          httr2::req_perform() |>
          httr2::resp_body_json()
      },
      silent = TRUE
    )

    # If error occurred, exit the loop
    if (inherits(collections_response, "try-error")) {
      cli::cli_warn(
        "Failed to fetch collections: {attr(collections_response, 'condition')$message}"
      )
      break
    }

    # Validate response structure
    if (
      is.null(collections_response) ||
        !is.list(collections_response) ||
        is.null(collections_response$collections)
    ) {
      cli::cli_warn("Invalid collections response structure")
      break
    }

    # Check if we got any collections
    if (length(collections_response$collections) == 0) {
      break
    }

    # Add collections to our list
    all_collections <- c(all_collections, collections_response$collections)

    # Early exit if we found the target collection
    if (!is.null(target_collection)) {
      collection_ids <- purrr::map_chr(all_collections, ~ .x$id)
      if (target_collection %in% collection_ids) {
        if (id_only) {
          return(collection_ids)
        } else {
          return(all_collections)
        }
      }
    }

    # Look for next link
    next_url <- NULL
    if (!is.null(collections_response$links)) {
      next_link <- purrr::detect(collections_response$links, ~ .x$rel == "next")
      if (!is.null(next_link)) {
        next_url <- next_link$href
      }
    }

    # If no next link, we're done
    if (is.null(next_url)) {
      break
    }

    # Update collections_url for next iteration
    collections_url <- next_url

    # Safety check to prevent infinite loops
    if (length(all_collections) > 10000) {
      cli::cli_warn("Stopped after retrieving 10,000+ collections")
      break
    }
  }

  # Return based on id_only parameter
  if (id_only) {
    return(purrr::map_chr(all_collections, ~ .x$id))
  } else {
    return(all_collections)
  }
}


#' @title Query a STAC source
#' @description A set of helper functions to query STAC APIs. These are very
#' thin wrappers around existing \pkg{rstac} functions, but provide some
#' convenience for common tasks.
#' @param bbox A numeric vector of length 4 representing a bounding box
#' @param stac_source The STAC source to query
#' @param collection The collection to query
#' @param start_date The start date for the query
#' @param end_date The end date for the query
#' @param assets A character vector of the asset names to include
#' @param mpc_sign A logical indicating whether to sign the items using the
#' Planetary Computer API's signing method (only required if using the
#' Planetary Computer STAC API).
#' @param drop_duplicates A logical indicating whether to drop duplicate items
#' from the collection. Duplicates are identified based on identical bounding
#' boxes (with a precision of 4 decimal places) and datetime properties.
#' Duplicate items in the Microsoft Planetary Computer STAC API are a known
#' issue.
#' @param check_collection A logical, should the collection name be checked
#' against available collections in the `stac_source`? If FALSE,  the collection
#' is assumed to exist.
#' @return A \pkg{rstac} doc_items object
#' @rdname stac_utilities
#' @export
#' @examplesIf interactive()
#' s2_its <- stac_query(
#'   bbox = c(-12.386, -37.214, -12.186, -37.014),
#'   stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
#'   collection = "sentinel-2-l2a",
#'   start_date = "2023-01-01",
#'   end_date = "2023-01-31"
#' )
#'
#' # For Microsoft Planetary Computer (MPC) assets, sign the the items using the
#' # MPC signing `sign_mpc_items()`:
#' # or use GDAL by setting the following environment variable:
#' # Sys.setenv("VSICURL_PC_URL_SIGNING" = "YES")
#'
#' @details Using these functions to produce a `doc_items` object is optional;
#' if the user prefers the greater flexibility of using \pkg{rstac} directly,
#' they are encouraged to do so. These functions are only intended to provide a
#' convenient way to interact with STAC APIs.
#'
#' The Microsoft Planetary Computer (MPC) STAC API is a very convenient, large
#' and open catalogue of EO data. To access these assets, urls must be signed -
#' this can be done using \code{\link{sign_mpc_items}},
#' \code{\link[rstac]{items_sign_planetary_computer}}`,
#' or with GDAL by setting the environment variable `VSICURL_PC_URL_SIGNING` to
#' `YES`. see the examples for more details.

stac_query <- function(
  bbox,
  stac_source,
  collection,
  start_date,
  end_date,
  assets = NULL,
  mpc_sign = grepl(
    "planetarycomputer\\.microsoft\\.com",
    stac_source
  ),
  drop_duplicates = TRUE,
  check_collection = TRUE
) {
  validate_bbox(bbox)
  v_assert_type(stac_source, "stac_source", "character")
  v_assert_type(collection, "collection", "character")
  v_assert_type(start_date, "start_date", "character")
  v_assert_type(end_date, "end_date", "character")
  v_assert_type(check_collection, "check_collection", "logical")

  if (!is.null(start_date)) {
    datetime <- paste0(
      format_stac_date(start_date),
      "/",
      format_stac_date(end_date)
    )
  } else {
    datetime <- NULL
  }

  # set stac endpoint
  stac_get <- rstac::stac(stac_source)

  # check collection against available collections
  if (check_collection) {
    # Get all collections with pagination (early exit if target found)
    coll_list <- get_all_collections(
      stac_source,
      target_collection = collection,
      id_only = TRUE
    )
    collection <- rlang::arg_match(collection, coll_list)
  }

  # perform search
  search <- rstac::stac_search(
    stac_get,
    collections = collection,
    bbox = bbox,
    datetime = datetime,
    limit = 999
  )
  stac_its <- rstac::items_fetch(execute_stac_request(search))

  if (drop_duplicates) {
    stac_its <- stac_drop_duplicates(stac_its)
  }

  if (!is.null(assets)) {
    assets <- rlang::arg_match(
      assets,
      rstac::items_assets(stac_its),
      multiple = TRUE
    )
    stac_its <- rstac::assets_select(stac_its, asset_names = assets)
  }

  if (mpc_sign) {
    stac_its <- sign_mpc_items(stac_its)
  }
  return(stac_its)
}


#' @rdname stac_utilities
#' @export
#' @examplesIf interactive()
#' sentinel2_stac_query(
#'   bbox = c(-12.386, -37.214, -12.186, -37.014),
#'   start_date = "2023-01-01",
#'   end_date = "2023-01-31",
#'   max_cloud_cover = 10,
#'   assets = c("B02", "B03", "B04", "B08", "SCL")
#' )
#' @details `sentinel2_stac_query` facilitates the querying of the Sentinel-2
#' level 2A data from the Microsoft Planetary Computer STAC API (or other S2
#' STAC sources) catalogue to generate a Sentinel 2 rstac doc_items object.
#' This is a convenience function that saves you needing to remember the exact
#' stac source and name but is simply a thin wrapper around `stac_query` and
#' `stac_cloud_filter`. Checks are not performed on the stac source or
#' collection name; assets are checked following the collection query and will
#' fail if requested assets are not present in the collection. Note that some
#' STAC sources use different names for the Sentinel-2 level 2A assets and the
#' provided defaults may not work for all sources.
sentinel2_stac_query <- function(
  bbox,
  start_date,
  end_date,
  assets = c(
    "B01",
    "B02",
    "B03",
    "B04",
    "B05",
    "B06",
    "B07",
    "B08",
    "B8A",
    "B09",
    "B11",
    "B12",
    "SCL"
  ),
  max_cloud_cover = 10,
  stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
  collection = "sentinel-2-l2a"
) {
  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    collection = collection[1],
    start_date = start_date,
    end_date = end_date,
    assets = assets
  )

  if (!is.null(max_cloud_cover)) {
    stac_its <- stac_cloud_filter(stac_its, max_cloud_cover)
  }

  return(stac_its)
}

#' @rdname stac_utilities
#' @examplesIf interactive()
#' hls_query <- hls_stac_query(
#'   c(144.130, -7.725, 144.470, -7.475),
#'   start_date = "2023-01-01",
#'   end_date = "2023-12-31",
#'   assets = c("B04", "B03", "B02", "Fmask"),
#'   collection = "HLSS30_2.0",
#'   max_cloud_cover = 35
#' )
#' # in order to download these items (or call further vrt_x functions) you
#' # will first need to set your credentials. The easiest way to do this is with
#' # the `earthdatalogin` package. First set your EARTHDATA_USER and
#' # EARTHDATA_PASSWORD environment variables and then run the following command:
#'
#' earthdatalogin::edl_netrc(
#'   username = Sys.getenv("EARTHDATA_USER"),
#'   password = Sys.getenv("EARTHDATA_PASSWORD")
#' )
#' @details
#' The `hls_stac_query` function generates a Harmonized Landsat Sentinel (HLS)
#' stac collection doc_items object. In order to access HLS data you will need a
#' NASA Earthdata account. You can create one at
#' \url{https://urs.earthdata.nasa.gov/users/new}. Once you have
#' an account, you can set your credentials using the `earthdatalogin` package
#' as shown in the examples.
#' You can alternatively use the Microsoft Planetary Computer STAC API to access
#' HLS data, which does not require an Earthdata account. Note that you must
#' specify the collection as `hls2-s30` or `hls2-l30` when using the
#' Planetary Computer STAC API and that the data is only available from 2020
#' onwards.
#' @export
hls_stac_query <- function(
  bbox,
  start_date,
  end_date,
  assets = c(
    "B01",
    "B02",
    "B03",
    "B04",
    "B05",
    "B06",
    "B07",
    "B08",
    "B8A",
    "B09",
    "B10",
    "B11",
    "B12",
    "Fmask"
  ),
  max_cloud_cover = 10,
  stac_source = c(
    "https://planetarycomputer.microsoft.com/api/stac/v1/",
    "https://cmr.earthdata.nasa.gov/stac/LPCLOUD/"
  ),
  collection = c("hls2-s30", "hls2-l30", "HLSS30_2.0", "HLSL30_2.0")
) {
  stac_source <- rlang::arg_match(stac_source)
  v_asset_hls_catalog(stac_source, collection[1])

  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    collection = collection,
    start_date = start_date,
    end_date = end_date,
    assets = assets
  )

  if (!is.null(max_cloud_cover)) {
    stac_its <- stac_cloud_filter(stac_its, max_cloud_cover)
  }
}


#' Generate a Sentinel 1 stac collection doc_items object
#' @param orbit_state A character vector of the orbit state to filter by. Both
#' arguments are allowed however - consider carefully the implications of
#' retaining both orbits if you intend to composite the data.
#' @rdname stac_utilities
#' @export
#' @examplesIf interactive()
#' sentinel1_stac_query(
#'   bbox = c(-12.386, -37.214, -12.186, -37.014),
#'   start_date = "2023-01-01",
#'   end_date = "2023-01-31",
#'   assets = "vv"
#' )
#' @details
#' The `sentinel1_stac_query` function generates a Sentinel 1 stac collection
#' doc_items object. It allows you to query Sentinel 1 data from the Planetary
#' Computer STAC API. The function returns a collection of Sentinel 1 items
#' that can be used to generate a Sentinel 1 rstac doc_items object.
sentinel1_stac_query <- function(
  bbox,
  start_date,
  end_date,
  assets = c("hh", "hv", "vh", "vv"),
  orbit_state = c("descending", "ascending"),
  stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
  collection = c("sentinel-1-rtc", "sentinel-1-grd")
) {
  orbit_state <- rlang::arg_match(orbit_state, multiple = TRUE)
  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    collection = collection[1],
    start_date = start_date,
    end_date = end_date,
    assets = assets
  )

  stac_its <- stac_orbit_filter(stac_its, orbit_state)

  return(stac_its)
}


#' Filter a STAC item collection by cloud cover
#' @param items An \pkg{rstac} doc_items object
#' @param max_cloud_cover A numeric value of the maximum cloud cover percentage
#' @rdname stac_utilities
#' @export
#' @details
#' The `stac_cloud_filter` function filters a STAC item collection by cloud cover
#' percentage. It uses the `eo:cloud_cover` property to filter the items.
#' Items with a cloud cover percentage less than the specified `max_cloud_cover`
#' are retained.
stac_cloud_filter <- function(items, max_cloud_cover) {
  v_assert_type(items, "items", "doc_items")
  v_assert_type(max_cloud_cover, "max_cloud_cover", "numeric")
  items |>
    rstac::items_filter(
      filter_fn = function(x) x$properties$`eo:cloud_cover` < max_cloud_cover
    )
}


#' Filter a STAC item collection by orbit state
#' @param items A \pkg{rstac} doc_items object
#' @param orbit_state A character string of the orbit state to filter by
#' @rdname stac_utilities
#' @export
#' @details
#' The `stac_orbit_filter` function filters a STAC item collection by orbit
#' state. It uses the `sat:orbit_state` property to filter the items.
#' Items with an orbit state matching the specified `orbit_state` are retained.
#' It is intended for use with Sentinel-1 data.
stac_orbit_filter <- function(
  items,
  orbit_state = c("descending", "ascending")
) {
  rlang::arg_match(orbit_state, multiple = TRUE)
  items |>
    rstac::items_filter(
      filter_fn = function(x) {
        x$properties$`sat:orbit_state` %in% orbit_state
      }
    )
}

#' Filter a STAC item collection by minimum coverage of a bounding box
#' @param items A \pkg{rstac} doc_items object
#' @param bbox A numeric vector of length 4 representing a bounding box
#' @param min_coverage A numeric value between 0 and 1 representing the minimum
#' coverage of the bounding box required to retain the item
#' @rdname stac_utilities
#' @export
#' @details
#' The `stac_coverage_filter` function filters a STAC item collection by
#' minimum coverage of a bounding box. It calculates the area of intersection
#' between the bounding box and the item's bounding box, and retains items
#' where the area of intersection is greater than the specified `min_coverage`
#' of the bounding box area.
stac_coverage_filter <- function(items, bbox, min_coverage = 0.5) {
  v_assert_type(items, "items", "doc_items", nullok = FALSE)
  v_assert_type(bbox, "bbox", "numeric")
  v_assert_length(bbox, "bbox", 4)
  v_assert_type(min_coverage, "min_coverage", "numeric")
  v_assert_within_range(min_coverage, "min_coverage", 0, 1)

  target_wkt <- gdalraster::bbox_to_wkt(bbox)
  target_area <- gdalraster::g_geodesic_area(target_wkt, srs = "EPSG:4326")
  rstac::items_filter(
    items,
    filter_fn = function(x) {
      intersect_area <- gdalraster::g_intersection(
        target_wkt,
        gdalraster::bbox_to_wkt(x$bbox)
      ) |>
        gdalraster::g_geodesic_area(srs = "EPSG:4326")

      (intersect_area / target_area) > min_coverage
    }
  )
}


#' Sign STAC items retrieved from the Microsoft Planetary Computer (MPC)
#'
#' @param items A STACItemCollection.
#' @param subscription_key A subscription key associated with your MPC account.
#' This key will be automatically used if the environment variable `MPC_TOKEN`
#' is set.
#' @details Microsoft no longer provide the ability to generate your own
#' subscription keys. This can be requested depending on the application; see
#' here for further details:
#' https://github.com/microsoft/PlanetaryComputer/issues/457. A key is not
#' required for data access and a more reliable way to ensure access is to
#' collocate the workload on the Azure West Europe region.
#'
#' Unlike the `rstac::items_sign_planetary_computer` function, this function
#' caches collection-level signing tokens for 45 minutes to avoid repeated
#' requests for the same collection within a short time period. This can avoid
#' 429 (permission denied) errors when signing many items from the same
#' collection.
#'
#' @returns A STACItemCollection object with signed assets url.
#'
#' @export
#' @rdname stac_utilities
sign_mpc_items <- function(
  items,
  subscription_key = Sys.getenv("MPC_TOKEN", unset = NA)
) {
  v_assert_type(items, "items", "doc_items", nullok = FALSE)
  if (rlang::is_empty(items$features)) {
    cli::cli_warn("There are no MPC items to sign.", class = "No_items_to_sign")
    return(items)
  }

  collection <- items$features[[1]]$collection

  token_cache_file <- set_token_cache(collection)

  reuse_token <- is_token_valid(token_cache_file)
  # browser()

  collection_token <- read_token(
    reuse_token,
    collection,
    subscription_key,
    token_cache_file
  )

  # update the href for each asset in each item
  for (i in seq_along(items$features)) {
    for (j in seq_along(items$features[[i]]$assets)) {
      items$features[[i]]$assets[[j]]$href <- paste0(
        items$features[[i]]$assets[[j]]$href,
        "?",
        collection_token$token
      )
    }
  }
  return(items)
}

#' Create a cache file path for an MPC token
#' @param collection A character string of the collection name
#' @return A character string of the cache file path
#' @noRd
#' @keywords internal
set_token_cache <- function(collection) {
  fs::dir_create(rappdirs::user_cache_dir("vrtility"))
  fs::path(
    rappdirs::user_cache_dir("vrtility"),
    paste0(collection, "_mpc_token.rds")
  )
}

#' Check if an MPC token is still valid and if it exists
#' @param token_cache_file A character string of the cache file path
#' @return A logical indicating whether the token is valid
#' @noRd
#' @keywords internal
is_token_valid <- function(token_cache_file) {
  if (fs::file_exists(token_cache_file)) {
    collection_token <- readRDS(token_cache_file)

    token_age <- lubridate::ymd_hms(Sys.time(), tz = Sys.timezone()) -
      lubridate::ymd_hms(collection_token$`msft:expiry`, tz = "UTC")

    if (token_age < 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' Read or request a new MPC token
#' @param reuse_token A logical indicating whether to reuse the token
#' @param collection A character string of the collection name
#' @param subscription_key A subscription key associated with your MPC account
#' @param token_cache_file A character string of the cache file path
#' @return A list containing the token and its expiry time
#' @noRd
#' @keywords internal
read_token <- function(
  reuse_token,
  collection,
  subscription_key,
  token_cache_file
) {
  if (!reuse_token) {
    collection_sign_url <- paste0(
      "https://planetarycomputer.microsoft.com/api/sas/v1/token/",
      collection
    )

    if (is.na(subscription_key)) {
      header_fun <- function(x) {
        httr2::req_headers(x, Accept = "application/json")
      }
    } else {
      header_fun <- function(x) {
        httr2::req_headers(
          x,
          Accept = "application/json",
          "Ocp-Apim-Subscription-Key" = subscription_key
        )
      }
    }

    collection_token <- httr2::request(collection_sign_url) |>
      header_fun() |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    saveRDS(
      collection_token,
      file = token_cache_file
    )
  } else {
    collection_token <- readRDS(token_cache_file)
  }
  return(collection_token)
}

#' Check for duplicate items in a STAC item collection
#' @param x A \pkg{rstac} doc_items object
#' @return An integer vector of the indices of unique items
#' @noRd
#' @keywords internal
check_for_dups <- function(x) {
  # Handle empty features
  if (rlang::is_empty(x$features)) {
    return(integer(0))
  }

  bbox_dttm_list <- purrr::map(x$features, \(feature) {
    list(
      platform = feature$properties$platform %||% "",
      bbox = round(feature$bbox, 4),
      datetime = feature$properties$datetime %||% "",
      orbit_state = feature$properties$`sat:orbit_state` %||% ""
    )
  })

  # Convert to a more comparable format
  bbox_dttm_df <- purrr::map(
    bbox_dttm_list,
    \(item) {
      data.frame(
        platform = item$platform,
        bbox_str = paste(item$bbox, collapse = ","),
        datetime = item$datetime,
        orbit_state = item$orbit_state
      )
    }
  ) |>
    purrr::list_rbind(names_to = "feature_index")

  # Find duplicates
  which(
    !duplicated(bbox_dttm_df[, c(
      "bbox_str",
      "datetime",
      "platform",
      "orbit_state"
    )])
  )
}

#' Remove duplicate items from a STAC item collection
#' @param items A \pkg{rstac} doc_items object
#' @return A \pkg{rstac} doc_items object with duplicate items removed
#' @rdname stac_utilities
#' @export
#' @details
#' The `stac_drop_duplicates` function removes duplicate items from a STAC item
#' collection. Duplicates are identified based on identical bounding boxes (
#' with a precision of 4 decimal places), datetime properties and platform.
#' Duplicate items in the Microsoft Planetary Computer STAC API are a known
#' issue.
stac_drop_duplicates <- function(items) {
  v_assert_type(items, "items", "doc_items", nullok = FALSE)
  if (rlang::is_empty(items$features)) {
    cli::cli_warn("There are no items from which to drop duplicates.")
    return(items)
  }
  rstac::items_select(items, check_for_dups(items))
}
