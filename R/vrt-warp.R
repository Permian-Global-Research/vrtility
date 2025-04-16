#' Construct A warped VRT or warped VRT collection.
#' @param x A vrt_collection or vrt_block (most likely the former).
#' @param t_srs character target SRS must be a numeric EPSG code, or SRS like
#' character such as a proj4 string or WKT.
#' @param te numeric vector of the target extent in the form
#' c(xmin, ymin, xmax, ymax) using the same SRS as in `t_srs`.
#' @param tr numeric vector of the target resolution in the form c(xres, yres)
#' @param resampling character vector of the resampling methods to be used for
#' each band. The default is "bilinear". "near" sampling will be used for the
#' mask_band if provided.
#' @param quiet logical indicating whether to suppress progress bar.
#' @rdname vrt_warp
#' @export
#' @details This function generates warped VRT objects types. This is
#' particularly useful when we want to create a vrt_stack but our input images
#' span multiple spatial reference systems. In such a situation, before warping
#' our input data we must align with our desired oputput grid.
#' @examples
#' s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#' ex_collect <- vrt_collect(s2files)
#' t_block <- ex_collect[[1]][[1]]
#' vrt_warp(
#' ex_collect,
#'    t_srs = t_block$srs,
#'    te = t_block$bbox,
#'    tr = t_block$res
#'  )
vrt_warp <- function(
  x,
  t_srs,
  te,
  tr,
  resampling,
  quiet
) {
  v_assert_type(t_srs, "t_srs", "character")
  v_assert_type(te, "te", "numeric")
  v_assert_length(te, "te", 4)
  v_assert_type(tr, "tr", "numeric")
  UseMethod("vrt_warp")
}

#' @noRd
#' @export
vrt_collect.default <- function(x, ...) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_collect()')}
    not implemented for class {class(x)[1]}"
  )
}

#' @rdname vrt_warp
#' @export
vrt_warp.vrt_block <- function(
  x,
  t_srs,
  te,
  tr,
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
  tr <- v_assert_res(tr)
  resampling <- rlang::arg_match(resampling)

  mask_band <- x$mask_band_name
  assets <- x$assets
  dttm <- x$date_time

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

  resamp_methods <- rep(resampling, length(assets))
  resamp_methods[mas_band_idx] <- "near"
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  vrt_save(x, tf)
  vrtwl <- purrr::map2_chr(
    seq_along(assets),
    resamp_methods,
    function(.x, .y) {
      vrt_to_warped_vrt(tf, .x, t_srs, te, tr, .y)
    }
  )

  outtf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  # browser()

  gdalraster::buildVRT(
    outtf,
    vrtwl,
    cl_arg = c(
      "-separate"
    ),
    quiet = TRUE
  )

  outtf <- set_vrt_descriptions(
    x = outtf,
    assets,
    as_file = TRUE
  )

  if (inherits(x, "vrt_stack")) {
    dttm_lab <- paste0("datetime_", seq_along(x$date_time))
  } else {
    dttm_lab <- "datetime"
  }
  if (is.null(mask_band)) {
    outtf <- set_vrt_metadata(
      outtf,
      keys = dttm_lab,
      values = dttm,
      as_file = TRUE
    )
  } else {
    outtf <- set_vrt_metadata(
      outtf,
      keys = c(dttm_lab, "mask_band_name"),
      values = c(dttm, mask_band),
      as_file = TRUE
    )
  }

  build_vrt_block(
    outtf,
    mask_fun = x$maskfun,
    pix_fun = x$pixfun,
    warped = TRUE
  )
}


#' @rdname vrt_warp
#' @export
vrt_warp.vrt_collection <- function(
  x,
  t_srs,
  te,
  tr,
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
  v_assert_length(tr, "tr", 2)
  resampling <- rlang::arg_match(resampling)

  warped_blocks <- purrr::map(
    x[[1]],
    ~ vrt_warp(.x, t_srs, te, tr, resampling, quiet)
  )
  # browser()

  build_vrt_collection(
    warped_blocks,
    pixfun = x$pixfun,
    maskfun = x$maskfun,
    warped = TRUE
  )
}

#' Create a warped VRT from a source raster
#' @param src character path to the source raster
#' @param t_srs character target SRS
#' @return character path to the warped VRT
#' @keywords internal
#' @noRd
#' @details A wrapper to simplify the process of creating a warped VRT from a
#' source raster. functionality here is deliberately limited. could expand if
#' needed. Issue we have is with srcs with different srs. This function enables
#' us to transform the srs right at the start of the vrt blocking process.
vrt_to_warped_vrt <- function(
  src,
  band,
  t_srs,
  te,
  tr = NULL,
  resampling = "bilinear"
) {
  tfw <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

  call_gdal_warp(
    src,
    tfw,
    t_srs,
    cl_arg = c(
      "-b",
      band,
      "-r",
      resampling,
      "-te",
      te,
      if (!is.null(tr)) c("-tr", tr) else NULL
    ),
    config_options = getOption("vrt.gdal.config.options"),
    quiet = TRUE
  )
}
