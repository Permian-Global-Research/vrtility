#' Construct the base VRT object for composing VRT pipelines.
#' @param x An object to be used to create a vrt_x object see details.
#' @param t_srs character target SRS must be a numeric EPSG code, or SRS like
#' character such as a proj4 string or WKT.
#' @param te numeric vector of the target extent in the form
#' c(xmin, ymin, xmax, ymax) using the same SRS as in `t_srs`.
#' @param tr numeric vector of the target resolution in the form c(xres, yres)
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
  x,
  t_srs,
  te,
  tr,
  mask_band = NULL,
  resampling = c(
    "blinear",
    "near",
    "cubic",
    "cubicspline",
    "lanczos",
    "average",
    "rms",
    "mode",
    "max",
    "min",
    "med",
    "q1",
    "q3",
    "sum"
  ),
  quiet = TRUE
) {
  v_assert_type(t_srs, "t_srs", "character")
  v_assert_type(te, "te", "numeric")
  v_assert_length(te, "te", 4)
  v_assert_type(tr, "tr", "numeric")
  v_assert_type(
    mask_band,
    "mask_band",
    c("character", "numeric"),
    multiple = TRUE,
    nullok = TRUE
  )
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
  x,
  t_srs,
  te,
  tr,
  mask_band = NULL,
  resampling = c(
    "bilinear",
    "near",
    "cubic",
    "cubicspline",
    "lanczos",
    "average",
    "rms",
    "mode",
    "max",
    "min",
    "med",
    "q1",
    "q3",
    "sum"
  ),
  quiet = TRUE
) {
  if (length(tr) == 1) {
    tr <- c(tr, tr)
  }
  v_assert_length(tr, "tr", 2)
  resampling <- rlang::arg_match(resampling)

  assets <- rstac::items_assets(x)
  t_srs <- to_wkt(t_srs)

  if (!is.null(mask_band)) {
    if (is.character(mask_band)) {
      mas_band_idx <- which(assets == mask_band)
    } else {
      mas_band_idx <- mask_band
      mask_band <- assets[mas_band_idx]
      if (is.na(mask_band)) {
        cli::cli_abort(
          c(
            "!" = "The numeric band id for the image mask ({mas_band_idx})
            does not exist."
          )
        )
      }
    }
  } else {
    mas_band_idx <- NULL
  }

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

        srcs_vrt_warped <- purrr::map2_chr(
          srcs,
          seq_len(length(srcs)),
          function(.x, .y) {
            if (!is.null(mask_band) && .y == mas_band_idx) {
              resampling <- "near"
            }
            vrt_to_warped_vrt(.x, t_srs, te, tr, resampling)
          },
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
        )

        tf <- if (!is.null(mask_band)) {
          set_vrt_metadata(
            tf,
            keys = c("datetime", "mask_band_name"),
            values = c(dttm, mask_band),
            as_file = TRUE
          )
        } else {
          set_vrt_metadata(
            tf,
            keys = "datetime",
            values = dttm,
            as_file = TRUE
          )
        }

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
