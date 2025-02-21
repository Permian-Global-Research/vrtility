#' #' User agent for vrtility
#' #' @return A user agent string
#' #' @keywords internal
#' #' @noRd
vrtility_usr_agent <- function() {
  httr::user_agent(
    "vrtility (https://github.com/Permian-Global-Research/vrtility)"
  )
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
#' @export
stac_query <- function(
    bbox,
    stac_source,
    collection,
    start_date,
    end_date,
    limit = 999,
    ...) {
  validate_bbox(bbox)
  v_assert_type(stac_source, "stac_source", "character")
  v_assert_type(collection, "collection", "character")
  v_assert_type(start_date, "start_date", "character")
  v_assert_type(end_date, "end_date", "character")
  v_assert_type(limit, "limit", "numeric")


  if (!is.null(start_date)) {
    datetime <- paste0(start_date, "/", end_date)
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
#' @details copied from the `rsi` package
sign_planetary_computer <- function(
    items,
    subscription_key = Sys.getenv("rsi_pc_key")) {
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
