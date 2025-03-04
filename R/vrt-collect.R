#' Construct the base VRT object for composing VRT pipelines.
#' @param x An object to be used to create a vrt_x object see details.
#' @param t_srs character target SRS must be a numeric EPSG code, or SRS like
#' character such as a proj4 string or WKT.
#' @param te numeric vector of the target extent in the form
#' c(xmin, ymin, xmax, ymax) using the same SRS as in `t_srs`.
#' @param tr numeric vector of the target resolution in the form c(xres, yres)
#' @param quiet logical indicating whether to suppress progress bar.
#' @rdname vrt_classes
#' @export
#' @details For now the main way to create a vrt_collection object, which forms
#' the basis of the vrrt-based pipelines in vrtility is using a doc_items
#' object from the `rstac` package. For more info on how to create a doc_items
#' object see [sentinel2_stac_query()]. To build a vrt_stack object a vrt_collection
#' is required first. The vrt_collection object is essentially a list of warped
#' VRT files. When vrt_stack is called on a vrt_collection object, the VRT files
#' are stacked into a single VRT file where each band in the VRT contains the
#' files of multiple epochs. We can then composite or summarise these layers
#' using the pixel functions.
vrt_collect <- function(x, t_srs, te, tr, quiet = FALSE) {
  UseMethod("vrt_collect")
}

#' @noRd
#' @export
vrt_collect.default <- function(x, ...) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_collect()')}
    not implemented for class {class(x)[1]}"
  )
}

#' @rdname vrt_classes
#' @export
vrt_collect.doc_items <- function(x, t_srs, te, tr, quiet = FALSE) {
  assets <- rstac::items_assets(x)
  t_srs <- to_wkt(t_srs)

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

        srcs_vrt_warped <- purrr::map_chr(
          srcs,
          ~ vrt_to_warped_vrt(.x, t_srs, te, tr),
          .progress = !quiet
        )

        gdalraster::buildVRT(
          tf,
          srcs_vrt_warped,
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
