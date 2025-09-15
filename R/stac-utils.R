#' format a date for a STAC query
#' @param x A character string of a date
#' @return A character string of the date in the correct format
#' @noRd
#' @keywords internal
format_stac_date <- function(x) {
  format(lubridate::as_datetime(x), "%Y-%m-%dT%H:%M:%SZ")
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
#' @param limit The number of items to return
#' @return A \pkg{rstac} doc_items object
#' @rdname stac_utilities
#' @export
#' @examplesIf interactive()
#' s2_its <- stac_query(
#'   bbox = c(-12.386, -37.214, -12.186, -37.014),
#'   stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
#'   collection = "sentinel-2-l2a",
#'   start_date = "2023-01-01",
#'   end_date = "2023-01-31",
#'   limit = 10
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
  limit = 999
) {
  validate_bbox(bbox)
  v_assert_type(stac_source, "stac_source", "character")
  v_assert_type(collection, "collection", "character")
  v_assert_type(start_date, "start_date", "character")
  v_assert_type(end_date, "end_date", "character")
  v_assert_type(limit, "limit", "numeric")

  if (!is.null(start_date)) {
    datetime <- paste0(
      format_stac_date(start_date),
      "/",
      format_stac_date(end_date)
    )
  } else {
    datetime <- NULL
  }

  search <- rstac::stac_search(
    rstac::stac(stac_source),
    collections = collection,
    bbox = bbox,
    datetime = datetime,
    limit = limit
  )

  return(rstac::items_fetch(rstac::get_request(search)))
}


#' Generate a Sentinel 2 stac collection doc_items object
#' @param bbox A numeric vector of the bounding box (length 4) in lat/long
#' @param assets A character vector of the asset names to include
#' @param max_cloud_cover A numeric value of the maximum cloud cover percentage
#' @param stac_source A character string of the STAC source
#' @param collection A character string of the collection to query
#' @param mpc_sign A logical indicating whether to sign the items using the
#' Planetary Computer API's signing method (only required if using the
#' Planetary Computer STAC API).
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
#' level 2A data from the Planetary Computer STAC API. It returns a
#' catalogue to generate a Sentinel 2 rstac doc_items object.
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
  collection = "sentinel-2-l2a",
  mpc_sign = TRUE,
  limit = 999
) {
  assets <- rlang::arg_match(assets, multiple = TRUE)
  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    start_date = start_date,
    end_date = end_date,
    collection = collection,
    limit = limit
  )

  if (!is.null(max_cloud_cover)) {
    stac_its <- stac_cloud_filter(stac_its, max_cloud_cover)
  }

  stac_its <- rstac::assets_select(stac_its, asset_names = assets)

  if (mpc_sign) {
    stac_its <- sign_mpc_items(stac_its)
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
  stac_source = "https://cmr.earthdata.nasa.gov/stac/LPCLOUD/",
  collection = c("HLSS30_2.0", "HLSL30_2.0"),
  limit = 999
) {
  assets <- rlang::arg_match(assets, multiple = TRUE)
  collection <- rlang::arg_match(collection)
  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    start_date = start_date,
    end_date = end_date,
    collection = collection,
    limit = limit
  )

  if (!is.null(max_cloud_cover)) {
    stac_its <- stac_cloud_filter(stac_its, max_cloud_cover)
  }

  rstac::assets_select(stac_its, asset_names = assets)
}


hls_mpc_stac_query <- function(
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
  mpc_sign = TRUE,
  stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
  collection = c("hls2-s30", "hls2-l30"),
  limit = 999
) {
  assets <- rlang::arg_match(assets, multiple = TRUE)
  collection <- rlang::arg_match(collection)
  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    start_date = start_date,
    end_date = end_date,
    collection = collection,
    limit = limit
  )

  if (!is.null(max_cloud_cover)) {
    stac_its <- stac_cloud_filter(stac_its, max_cloud_cover)
  }

  stac_its <- rstac::assets_select(stac_its, asset_names = assets)

  if (mpc_sign) {
    stac_its <- sign_mpc_items(stac_its)
  }
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
  collection = c("sentinel-1-rtc", "sentinel-1-grd"),
  mpc_sign = TRUE,
  limit = 999
) {
  assets <- rlang::arg_match(assets, multiple = TRUE)
  orbit_state <- rlang::arg_match(orbit_state, multiple = TRUE)
  collection <- rlang::arg_match(collection)

  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    start_date = start_date,
    end_date = end_date,
    collection = collection,
    limit = limit
  )

  stac_its <- stac_orbit_filter(stac_its, orbit_state)

  stac_its <- rstac::assets_select(stac_its, asset_names = assets)

  if (mpc_sign) {
    stac_its <- sign_mpc_items(stac_its)
  }

  return(stac_its)
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
  if (rlang::is_empty(items$features)) {
    cli::cli_abort("There are no MPC items to sign.")
  }

  collection <- items$features[[1]]$collection

  token_cache_file <- set_token_cache(collection)

  reuse_token <- is_token_valid(token_cache_file)

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
      # cli::cli_alert_info("Using cached MPC token")
      return(TRUE)
    } else {
      # cli::cli_alert_info("MPC token expired - requesting new token")
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
