#' Construct the base VRT object for composing VRT pipelines.
#' @rdname vrt_collect
#' @export
vrt_collect <- function(x) {
  UseMethod("vrt_collect")
}

#' @noRd
#' @export
vrt_collect.default <- function(x) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_collect()')} not implemented for class {class(x)[1]}"
  )
}

#' @rdname vrt_collect
#' @export
vrt_collect.doc_items <- function(x) {
  x <- rstac::assets_select(x, asset_names = c("B02", "B03", "B04"))
  assets <- rstac::items_assets(x)

  vrt_items <- purrr::map(assets, function(a) {
    its_asset <- rstac::assets_select(x, asset_names = a)

    dttm <- rstac::items_datetime(its_asset)

    suppressWarnings(
      uri <- as.list(rstac::assets_url(its_asset, append_gdalvsi = TRUE))
    )
    purrr::map2(uri, dttm, ~ list(uri = .x, dttm = .y))
  }) |>
    purrr::set_names(assets) |>
    purrr::transpose() |>
    purrr::map(
      function(x) {
        srcs <- purrr::map_chr(x, ~ .x$uri)
        dttm <- unique(purrr::map_chr(x, ~ .x$dttm))

        if (length(dttm) > 1) {
          cli::cli_warn(
            c(
              "!" = "Multiple datetimes detected in for the following sources:",
              "{srcs}",
              "i" = "Using the first datetime."
            )
          )
          dttm <- dttm[1]
        }

        tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

        gdalraster::buildVRT(
          tf,
          srcs,
          cl_arg = c(
            "-separate"
          ),
          quiet = TRUE
        )

        tf <- set_vrt_descriptions(
          x = tf,
          names(x),
          as_file = TRUE
        ) |>
          set_vrt_metadata(
            keys = "datetime",
            values = dttm,
            as_file = TRUE
          )

        # read and verify modified VRT
        gdr <- new(gdalraster::GDALRaster, tf)
        assets <- purrr::map_chr(
          seq_along(x),
          function(.x) gdr$getDescription(.x)
        )
        no_data_val <- purrr::map_dbl(
          seq_along(x),
          function(.x) gdr$getNoDataValue(.x)
        )
        dttm <- gdr$getMetadataItem(0, "datetime", "")

        build_vrt_block(
          xml2::read_xml(tf),
          srs = gdr$getProjection(),
          bbox = gdr$bbox(),
          res = gdr$res(),
          date_time = dttm,
          assets = assets,
          no_data_val = no_data_val
        )
      }
    )

  bbox <- purrr::map(vrt_items, function(.x) .x$bbox) |>
    purrr::reduce(
      function(.x, .y) {
        c(
          min(.x[1], .y[1]),
          min(.x[2], .y[2]),
          max(.x[3], .y[3]),
          max(.x[4], .y[4])
        )
      }
    )

  sd <- purrr::map_chr(
    vrt_items,
    function(.x) .x$date_time
  ) |>
    lubridate::as_datetime()

  uniq_assets <- purrr::map(
    vrt_items,
    function(.x) .x$assets
  ) |>
    unlist() |>
    unique()

  uniq_crs <- purrr::map(
    vrt_items,
    function(.x) .x$srs
  ) |>
    unlist() |>
    unique()

  min_res <- purrr::map(
    vrt_items,
    function(.x) .x$res
  ) |>
    purrr::reduce(
      function(.x, .y) {
        c(
          min(.x[1], .y[1]),
          min(.x[2], .y[2])
        )
      }
    )

  build_vrt_collection(
    vrt_items,
    srs = uniq_crs,
    bbox = bbox,
    res = min_res,
    start_date = min(sd),
    end_date = max(sd),
    n_its = length(vrt_items),
    assets = uniq_assets
  )
}
