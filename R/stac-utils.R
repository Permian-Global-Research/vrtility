#' #' User agent for vrtility
#' #' @return A user agent string
#' #' @keywords internal
#' #' @noRd
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
#' @param ... Additional arguments to pass to the query
#' @return A list of items
#' @rdname stac_utilities
#' @export
stac_query <- function(
  bbox,
  stac_source,
  collection,
  start_date,
  end_date,
  limit = 999,
  ...
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


#' Sign STAC items retrieved from the Planetary Computer
#'
#' @param items A STACItemCollection.
#' @param subscription_key Optionally, a subscription key associated with your
#' Planetary Computer account. At the time of writing, this is required for
#' downloading Sentinel 1 RTC products, as well as NAIP imagery. This key will
#' be automatically used if the environment variable `rsi_pc_key` is set.
#'
#' @returns A STACItemCollection object with signed assets url.
#'
#' @export
#' @rdname stac_utilities
#' @details copied from the `rsi` package
sign_planetary_computer <- function(
  items,
  subscription_key = Sys.getenv("rsi_pc_key")
) {
  # check for the variable used by sits if the rsi one isn't set
  if (subscription_key == "") subscription_key <- Sys.getenv("MPC_TOKEN")
  if (subscription_key == "") {
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
#' @param start_date A character string of the start date
#' @param end_date A character string of the end date
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
  mpc_sign = TRUE
) {
  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    start_date = start_date,
    end_date = end_date,
    collection = collection
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
  collection = c("HLSS30_2.0", "HLSL30_2.0")
) {
  collection <- rlang::arg_match(collection)
  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    start_date = start_date,
    end_date = end_date,
    collection = collection
  )

  if (!is.null(max_cloud_cover)) {
    stac_its <- stac_cloud_filter(stac_its, max_cloud_cover)
  }

  rstac::assets_select(stac_its, asset_names = assets)
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
      filter_fn = \(x) x$properties$`eo:cloud_cover` < max_cloud_cover
    )
}
