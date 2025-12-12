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
#' @param lazy logical indicating whether to create virtual warped files (TRUE)
#' or to materialize the warped files to disk (FALSE). When woring with remote
#' data sources, lazy=FALSE is strongly recommended to improve performance.
#' When NULL (default) the function will decide based on whether the input
#' data is remote or local.
#' @inheritParams vrt_compute
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
  quiet = TRUE,
  lazy = NULL,
  creation_options = gdal_creation_options(
    COMPRESS = "NONE",
    PREDICTOR = NULL
  ),
  warp_options = gdalwarp_options(),
  config_options = gdal_config_options()
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
  quiet = TRUE,
  lazy = NULL,
  creation_options = gdal_creation_options(
    COMPRESS = "NONE",
    PREDICTOR = NULL
  ),
  warp_options = gdalwarp_options(),
  config_options = gdal_config_options()
) {
  tr <- v_assert_res(tr)
  resampling <- rlang::arg_match(resampling)

  daemon_setup()

  if (is.null(lazy)) {
    lazy <- !x$is_remote
  }

  warptab <- warp_setup(
    tr = tr,
    resampling = resampling,
    t_srs = t_srs,
    te = te,
    x = x,
    virtual = lazy,
    item_idx = 1
  )

  if (!lazy) {
    w_cl_arg <- combine_warp_opts(
      creation_options,
      warp_options
    )
  } else {
    w_cl_arg <- warp_options
  }

  src_df_warper(
    warptab,
    w_cl_arg,
    config_options,
    quiet
  )

  warped_block <- src_df_to_block(x, warptab, is_remote = x$is_remote && lazy)
  return(warped_block)
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
  quiet = TRUE,
  lazy = NULL,
  creation_options = gdal_creation_options(
    COMPRESS = "NONE",
    PREDICTOR = NULL
  ),
  warp_options = gdalwarp_options(),
  config_options = gdal_config_options()
) {
  v_assert_length(tr, "tr", 2)
  resampling <- rlang::arg_match(resampling)

  daemon_setup()

  any_remote <- any(purrr::map_lgl(x$vrt, function(vrt) {
    vrt$is_remote
  }))

  if (is.null(lazy)) {
    # TODO: note this assumed all remote if any are - generally fine. but may
    # unecessarily force materialization in some edge cases...
    lazy <- !any_remote
  }

  warptab <- purrr::imap(
    x[[1]],
    ~ warp_setup(
      tr = tr,
      resampling = resampling,
      t_srs = t_srs,
      te = te,
      x = .x,
      virtual = lazy,
      item_idx = .y
    )
  ) |>
    purrr::list_rbind()

  if (!lazy) {
    w_cl_arg <- combine_warp_opts(
      creation_options,
      warp_options
    )
  } else {
    w_cl_arg <- warp_options
  }

  src_df_warper(
    warptab,
    w_cl_arg,
    config_options,
    quiet
  )

  warptablist <- split(
    warptab,
    f = warptab$item_idx
  )

  warped_blocks <- purrr::map2(x[[1]], warptablist, function(xi, wt) {
    src_df_to_block(xi, wt, is_remote = any_remote && lazy)
  })

  build_vrt_collection(
    warped_blocks,
    pixfun = x$pixfun,
    maskfun = x$maskfun,
    warped = TRUE
  )
}


warp_setup <- function(tr, resampling, t_srs, te, x, virtual, item_idx) {
  mask_band <- x$mask_band_name
  assets <- x$assets

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

  # here should we also catch byte bands and force near resampling?
  resamp_methods <- rep(resampling, length(assets))
  resamp_methods[mas_band_idx] <- "near" # mask band should be nearest neighbour

  of <- fs::file_temp(
    pattern = glue::glue("warpfile_{seq_along(assets)}_"),
    tmp_dir = getOption("vrt.cache"),
    ext = if (virtual) "vrt" else "tif"
  )

  n_assets <- length(assets)

  data.frame(
    band = seq_along(assets),
    resampling = resamp_methods,
    src = rep(x$vrt_src, n_assets),
    tsrs = rep(t_srs, n_assets),
    te = I(rep(list(as.numeric(te)), n_assets)),
    tr = I(rep(list(as.numeric(tr)), n_assets)),
    outtf = of,
    item_idx = item_idx
  )
}


src_df_to_block <- function(x, warpdf, is_remote = FALSE) {
  outtf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

  gdalraster::buildVRT(
    outtf,
    warpdf$outtf,
    cl_arg = c(
      "-separate",
      src_block_size(warpdf$outtf[1])
    ),
    quiet = TRUE
  )

  outtf <- set_vrt_descriptions(
    x = outtf,
    descriptions = x$assets,
    as_file = TRUE
  )

  if (inherits(x, "vrt_stack")) {
    dttm_lab <- paste0("datetime_", seq_along(x$date_time))
  } else {
    dttm_lab <- "datetime"
  }
  if (is.null(x$mask_band)) {
    outtf <- set_vrt_metadata(
      outtf,
      keys = dttm_lab,
      values = x$date_time,
      as_file = TRUE
    )
  } else {
    outtf <- set_vrt_metadata(
      outtf,
      keys = c(dttm_lab, "mask_band_name"),
      values = c(x$date_time, x$mask_band),
      as_file = TRUE
    )
  }

  build_vrt_block(
    outtf,
    maskfun = x$maskfun,
    pixfun = x$pixfun,
    warped = TRUE,
    is_remote = is_remote
  )
}


src_df_warper <- function(warptab, w_cl_arg, config_options, quiet) {
  of <- purrr::pmap_chr(
    warptab,
    purrr::in_parallel(
      function(band, resampling, src, tsrs, te, tr, outtf, item_idx) {
        fs::path_ext(outtf)
        cclia <- c(
          w_cl_arg,
          "-b",
          band,
          "-r",
          resampling,
          "-te",
          te,
          "-tr",
          tr,
          if (
            "TILED=YES" %in% w_cl_arg && identical(fs::path_ext(outtf), "vrt")
          ) {
            src_block_size(src)
          } else {
            NULL
          }
        )

        compute_with_py_env(
          call_gdal_warp(
            src,
            outtf,
            tsrs,
            cl_arg = cclia,
            config_options = config_options,
            quiet = TRUE
          ),
          config_options = config_options
        )
      },
      call_gdal_warp = call_gdal_warp,
      config_options = config_options,
      src_block_size = src_block_size,
      w_cl_arg = w_cl_arg,
      compute_with_py_env = compute_with_py_env
    ),
    .progress = !quiet
  )
  return(invisible(of))
}
