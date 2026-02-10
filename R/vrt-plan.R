#' Create a VRT execution plan from STAC items
#'
#' @description
#' `vrt_plan()` creates a lightweight metadata structure from rstac items
#' without creating VRT files. This plan can be passed directly to
#' [vrt_warp()] for efficient processing, skipping the intermediate VRT
#' creation step that [vrt_collect()] performs.
#'
#' @param x A `doc_items` object from rstac.
#' @param vsi_prefix Character string specifying the GDAL virtual file system
#' prefix (e.g., "/vsicurl/", "/vsis3/"). Default is empty string which lets
#' rstac determine the appropriate prefix.
#' @param driver Character string specifying a driver prefix to prepend to
#' source paths.
#' @param ... Additional arguments (unused).
#'
#' @return A `vrt_plan` object containing:
#' \describe{
#'   \item{sources}{List of items, each containing source URLs per asset}
#'   \item{assets}{Character vector of asset/band names}
#'   \item{date_time}{Character vector of datetimes (one per item)}
#'   \item{n_items}{Number of items in the plan}
#'   \item{vsi_prefix}{VSI prefix used}
#'   \item{driver}{Driver prefix used}
#'   \item{config_options}{GDAL configuration options}
#' }
#'
#' @details
#' Unlike [vrt_collect()], `vrt_plan()` does not create any VRT files or make

#' GDAL calls to read spatial metadata. This makes it faster when you intend
#' to immediately warp the data to a common grid.
#'
#' The trade-off is that spatial metadata (CRS, bounding box, resolution) is
#' not available until warping occurs. Use [vrt_collect()] if you need to
#' inspect or use the source spatial properties before warping.
#'
#' @examples
#' \donttest{
#' # Query STAC items
#' items <- sentinel2_stac_query(
#'   bbox = c(-12.5, -37.5, -12.0, -37.0),
#'   start_date = "2023-01-01",
#'   end_date = "2023-01-31",
#'   max_cloud_cover = 10,
#'   assets = c("B02", "B03", "B04", "SCL")
#' )
#'
#' # Create plan (fast, no VRT creation)
#' plan <- vrt_plan(items)
#'
#' # Warp directly from plan
#' warped <- vrt_warp(
#'   plan,
#'   t_srs = "EPSG:32724",
#'   te = c(700000, 5850000, 710000, 5860000),
#'   tr = c(10, 10)
#' )
#' }
#'
#' @seealso [vrt_collect()] for full VRT creation, [vrt_warp()] for warping
#' @rdname vrt_plan
#' @export
vrt_plan <- function(
  x,
  vsi_prefix = "",
  driver = "",
  ...
) {
  UseMethod("vrt_plan")
}

#' @noRd
#' @export
vrt_plan.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "!" = "{class(x)[1]} objects are not supported for {.fn vrt_plan}.",
      "i" = "A {.cls doc_items} object from {.pkg rstac} is required."
    )
  )
}

#' @rdname vrt_plan
#' @export
vrt_plan.doc_items <- function(
  x,
  vsi_prefix = "",
  driver = "",
  ...
) {
  gdal_vrt_collect_arg_checks(vsi_prefix, driver)

  # Extract asset names, sorted numerically if possible
  assets <- rstac::items_assets(x)[order(as.numeric(gsub(
    "\\D",
    "",
    rstac::items_assets(x)
  )))]

  # Build source information for each asset and item
  items_uri <- purrr::map(assets, function(a) {
    its_asset <- rstac::assets_select(x, asset_names = a)

    dttm <- rstac::items_datetime(its_asset)

    suppressWarnings(
      uri <- as.list(gdal_driver_vsi_src_builder(
        rstac::assets_url(its_asset, append_gdalvsi = !nzchar(vsi_prefix)),
        vsi = vsi_prefix,
        drive = driver
      ))
    )
    purrr::map2(uri, dttm, ~ list(uri = .x, dttm = .y))
  }) |>
    purrr::set_names(assets) |>
    purrr::transpose()

  # Extract datetimes from the items
  date_times <- purrr::map_chr(items_uri, function(item) {
    dttm <- unique(purrr::map_chr(item, ~ .x$dttm))
    if (length(dttm) > 1) {
      dttm <- dttm[1]
    }
    dttm
  })

  # Sort by datetime
  sort_order <- order(lubridate::as_datetime(date_times))
  items_uri <- items_uri[sort_order]
  date_times <- date_times[sort_order]

  build_vrt_plan(
    sources = items_uri,
    assets = assets,
    date_time = date_times,
    vsi_prefix = vsi_prefix,
    driver = driver
  )
}


#' Internal constructor for vrt_plan objects
#' @keywords internal
#' @noRd
build_vrt_plan <- function(
  sources,
  assets,
  date_time,
  vsi_prefix,
  driver
) {
  plan <- list(
    sources = sources,
    assets = assets,
    date_time = date_time,
    n_items = length(sources),
    vsi_prefix = vsi_prefix,
    driver = driver
  )

  class(plan) <- c("vrt_plan", "list")
  plan
}


#' @export
print.vrt_plan <- function(x, ...) {
  cli::cli_inform(c(">" = cli::style_bold(cli::col_cyan("<VRT Plan>"))))

  cli::cli_inform(
    c(
      assets_printer(x$assets),
      dttm_printer(x$date_time, "start"),
      dttm_printer(x$date_time, "end"),
      n_items_printer(x$n_items),
      sources_printer(x$sources)
    )
  )

  invisible(x)
}


#' Print helper for source URLs
#' @keywords internal
#' @noRd
sources_printer <- function(x) {
  # Get first source URL as example
  first_item <- x[[1]]
  first_url <- first_item[[1]]$uri

  # Truncate if too long
  max_len <- 60
  if (nchar(first_url) > max_len) {
    display_url <- paste0(substr(first_url, 1, max_len - 3), "...")
  } else {
    display_url <- first_url
  }

  paste(
    cli::style_bold(cli::col_blue("Source pattern:")),
    display_url
  )
}
