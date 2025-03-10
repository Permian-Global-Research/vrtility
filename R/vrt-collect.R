#' Construct the base VRT object for composing VRT pipelines.
#' @param x An object to be used to create a vrt_x object see details.
#' @param t_srs character target SRS must be a numeric EPSG code, or SRS like
#' character such as a proj4 string or WKT.
#' @param te numeric vector of the target extent in the form
#' c(xmin, ymin, xmax, ymax) using the same SRS as in `t_srs`.
#' @param tr numeric vector of the target resolution in the form c(xres, yres)
#' @param mask_band character or numeric vector of the mask band name or index.
#' @param resampling character vector of the resampling methods to be used for
#' each band. The default is "bilinear". "near" sampling will be used for the
#' mask_band if provided.
#' @param quiet logical indicating whether to suppress progress bar.
#' @rdname vrt_collect
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
vrt_collect <- function(
  x
) {
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

#' @rdname vrt_collect
#' @export
vrt_collect.doc_items <- function(
  x
) {
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
        )

        tf <- set_vrt_metadata(
          tf,
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

#' Internal method for building a vrt_collection object
#' @keywords internal
#' @noRd
build_vrt_collection <- function(
  x,
  pixfun = NULL,
  maskfun = NULL,
  ...
) {
  bbox <- purrr::map(x, function(.x) .x$bbox) |>
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
    x,
    function(.x) .x$date_time
  )

  uniq_assets <- purrr::map(
    x,
    function(.x) .x$assets
  ) |>
    unlist() |>
    unique()

  uniq_crs <- purrr::map(
    x,
    function(.x) .x$srs
  ) |>
    unlist() |>
    unique()

  min_res <- purrr::map(
    x,
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

  mask_band_name <- purrr::map_chr(
    x,
    function(.x) .x$mask_band_name
  ) |>
    unique()

  rvrt <- list(
    vrt = x,
    srs = uniq_crs,
    bbox = bbox,
    res = min_res,
    date_time = sd,
    n_items = length(x),
    assets = uniq_assets,
    mask_band_name = mask_band_name,
    pixfun = pixfun,
    maskfun = maskfun
  )

  class(rvrt) <- c("vrt_collection", "vrt_block", "list")

  return(rvrt)
}


#' @export
#' @param xml logical indicating whether to print the XML of the VRT collection.
#' @param pixfun logical indicating whether to print the pixel function.
#' @param maskfun logical indicating whether to print the mask function.
#' @param blocks A logical indicating whether to print the blocks instead of
#' the collection summary.
#' @param ... Additional arguments not used
#' @rdname vrt_collect
print.vrt_collection <- function(
  x,
  xml = FALSE,
  pixfun = FALSE,
  maskfun = FALSE,
  blocks = FALSE,
  ...
) {
  if (blocks) {
    print(x[[1]])
    return(invisible(x))
  }
  cli::cli_inform(c(
    ">" = cli::style_bold(cli::col_green("<VRT Collection> \n"))
  ))

  if (!is.null(x$pixfun)) {
    if (pixfun) {
      pixel_fun_printer(x$pixfun)
    } else {
      pix_fun_print_msg()
    }
  }

  if (!is.null(x$maskfun)) {
    if (maskfun) {
      pixel_fun_printer(x$maskfun, type = "mf")
    } else {
      pix_fun_print_msg(type = "mf")
    }
  }

  cli::cli_inform(
    c(
      crs_printer(x$srs),
      bbox_printer(x$bbox),
      res_printer(x$res),
      dttm_printer(x$date_time, "start"),
      dttm_printer(x$date_time, "end"),
      n_items_printer(x$n_items),
      assets_printer(x$assets)
    )
  )
  invisible(x)
}
