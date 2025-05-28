#' User agent for vrtility
#' @return A user agent string
#' @keywords internal
#' @noRd
vrtility_usr_agent <- function() {
  httr::user_agent(
    "vrtility (https://github.com/Permian-Global-Research/vrtility)"
  )
}

#' format a date for a STAC query
#' @param x A character string of a date
#' @return A character string of the date in the correct format
#' @noRd
#' @keywords internal
format_stac_date <- function(x) {
  format(lubridate::as_datetime(x), "%Y-%m-%dT%H:%M:%SZ")
}


#' Query a STAC source
#' @param bbox A numeric vector of length 4 representing a bounding box
#' @param stac_source The STAC source to query
#' @param collection The collection to query
#' @param start_date The start date for the query
#' @param end_date The end date for the query
#' @param limit The number of items to return
#' @return A list of items
#' @rdname stac_utilities
#' @export
#' @examplesIf interactive()
#' s2_its <- stac_query(
#'  bbox = c(-12.386, -37.214, -12.186, -37.014),
#'  stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
#'  collection = "sentinel-2-l2a",
#'  start_date = "2023-01-01",
#'  end_date = "2023-01-31",
#'  limit = 10
#' )
#'
#' mpc_singed <- sign_planetary_computer(s2_its)
#'

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

  items <- rstac::items_fetch(
    rstac::get_request(
      search,
      vrtility_usr_agent()
    ),
    vrtility_usr_agent()
  )

  items
}


#' Sign STAC items retrieved from the Microsoft Planetary Computer (MPC)
#'
#' @param items A STACItemCollection.
#' @param subscription_key Optionally (but strongly recommended), a
#' subscription key associated with your MPC account. At the time of writing,
#' this is required for downloading Sentinel 1 RTC products, as well as NAIP
#' imagery. This key willb be automatically used if the environment
#' variable `MPC_TOKEN` is set.
#'
#' @returns A STACItemCollection object with signed assets url.
#'
#' @export
#' @rdname stac_utilities
sign_planetary_computer <- function(
  items,
  subscription_key = Sys.getenv("MPC_TOKEN")
) {
  if (subscription_key == "") {
    cli::cli_warn(
      c(
        "!" = "No subscription key provided. Using default signing method.",
        " " = "This may not work for some items or could be slower.",
        "i" = "Set your key using the `MPC_TOKEN` environment variable."
      )
    )
    rstac::items_sign(
      items,
      rstac::sign_planetary_computer(vrtility_usr_agent())
    )
  } else {
    rstac::items_sign(
      items,
      rstac::sign_planetary_computer(
        vrtility_usr_agent(),
        headers = c("Ocp-Apim-Subscription-Key" = subscription_key)
      )
    )
  }
}


#' Generate a Sentinel 2 stac collection doc_imes object
#' @param bbox A numeric vector of the bounding box (length 4) in lat/long
#' @param assets A character vector of the asset names to include
#' @param max_cloud_cover A numeric value of the maximum cloud cover percentage
#' @param stac_source A character string of the STAC source
#' @param collection A character string of the collection to query
#' @param mpc_sign A logical indicating whether to sign the items using the
#' Planetary Computer API's signing method (only required if using the
#' Planetary Computer STAC API).
#' @return A stac_vrt object
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
#'
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
    stac_its <- sign_planetary_computer(stac_its)
  }

  return(stac_its)
}

#' Generate a Harmonized Landsat Sentinel (HLS) stac collection doc_imes object
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
#' In order to access HLS data you will need a NASA Earthdata account. You can
#' create one at \url{https://urs.earthdata.nasa.gov/users/new}. Once you have
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
    stac_its <- sign_planetary_computer(stac_its)
  }
  return(stac_its)
}

#' Filter a STAC item collection by cloud cover
#' @param items A STACItemCollection
#' @param max_cloud_cover A numeric value of the maximum cloud cover percentage
#' @return A STACItemCollection
#' @noRd
#' @keywords internal
stac_cloud_filter <- function(items, max_cloud_cover) {
  items |>
    rstac::items_filter(
      filter_fn = function(x) x$properties$`eo:cloud_cover` < max_cloud_cover
    )
}

#' Filter a STAC item collection by orbit state
#' @param items A STACItemCollection
#' @param orbit_state A character string of the orbit state to filter by
#' @return A STACItemCollection
#' @noRd
#' @keywords internal
stac_orbit_filter <- function(items, orbit_state) {
  items |>
    rstac::items_filter(
      filter_fn = function(x) {
        x$properties$`sat:orbit_state` %in% orbit_state
      }
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
  if (collection == "sentinel-1-rtc") {
    if (Sys.getenv("MPC_TOKEN") == "") {
      cli::cli_abort(
        c(
          "!" = "No subscription key provided.",
          "i" = "To access the Sentinel 1 RTC collection, you need a 
          subscription key. Set your key using the `MPC_TOKEN` environment 
          variable."
        )
      )
    }
  }

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
    stac_its <- sign_planetary_computer(stac_its)
  }

  return(stac_its)
}
