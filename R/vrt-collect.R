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

        build_vrt_block(tf)
      }
    )

  build_vrt_collection(
    vrt_items
  )
}
