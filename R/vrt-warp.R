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
#' or to materialize the warped files to disk (FALSE). When working with remote
#' data sources, lazy=FALSE is strongly recommended to improve performance.
#' When NULL (default) the function will decide based on whether the input
#' data is remote or local.
#' @inheritParams vrt_compute
#' @rdname vrt_warp
#' @export
#' @details This function generates warped VRT objects types. This is
#' particularly useful when we want to create a vrt_stack but our input images
#' span multiple spatial reference systems. In such a situation, before warping
#' our input data we must align with our desired output grid.
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
vrt_warp.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "!" = "{.fn vrt_warp} is not implemented for class {.cls {class(x)[1]}}.",
      "i" = "A {.cls vrt_collection}, {.cls vrt_block}, or {.cls vrt_plan} object is required."
    )
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


#' @rdname vrt_warp
#' @export
vrt_warp.vrt_plan <- function(
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
  lazy = FALSE,
  creation_options = gdal_creation_options(
    COMPRESS = "NONE",
    PREDICTOR = NULL
  ),
  warp_options = gdalwarp_options(),
  config_options = gdal_config_options()
) {
  v_assert_length(tr, "tr", 2)
  resampling <- rlang::arg_match(resampling)

  daemon_setup(gdal_config = x$config_options)

  # Set GDAL config options
  orig_config <- set_gdal_config(x$config_options)
  on.exit(set_gdal_config(orig_config), add = TRUE)

  # Prepare warp options
  if (!lazy) {
    w_cl_arg <- combine_warp_opts(
      creation_options,
      warp_options
    )
  } else {
    w_cl_arg <- warp_options
  }

  # Build VRTs and warp in parallel for each item
  plan_assets <- x$assets

  warped_blocks <- purrr::imap(
    x$sources,
    purrr::in_parallel(
      function(item, item_idx) {
        # Extract source URLs for this item
        srcs <- purrr::map_chr(item, ~ .x$uri)
        dttm <- unique(purrr::map_chr(item, ~ .x$dttm))
        if (length(dttm) > 1) {
          dttm <- dttm[1]
        }

        # Build temporary VRT from sources
        tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

        gdalraster::buildVRT(
          tf,
          srcs,
          cl_arg = c(
            "-separate",
            src_block_size(srcs[1])
          ),
          quiet = TRUE
        )

        tf <- set_vrt_descriptions(
          x = tf,
          descriptions = plan_assets,
          as_file = TRUE
        )

        # Create minimal block-like structure for warp_setup
        mini_block <- list(
          vrt_src = tf,
          assets = plan_assets,
          date_time = dttm,
          mask_band_name = NULL,
          mask_band = NULL,
          maskfun = NULL,
          pixfun = NULL
        )

        # Detect byte bands for nearest neighbor resampling
        byte_band_idx <- detect_byte_bands(tf)

        # Create warp table for this item
        warptab <- warp_setup(
          tr = tr,
          resampling = resampling,
          t_srs = t_srs,
          te = te,
          x = mini_block,
          virtual = lazy,
          item_idx = item_idx,
          byte_band_idx = byte_band_idx
        )

        src_df_warper(
          warptab,
          w_cl_arg,
          config_options,
          quiet = quiet
        )

        # Convert to vrt_block
        src_df_to_block(mini_block, warptab, is_remote = TRUE)
      },
      # Export functions and variables to parallel workers
      set_vrt_descriptions = set_vrt_descriptions,
      src_block_size = src_block_size,
      warp_setup = warp_setup,
      src_df_to_block = src_df_to_block,
      call_gdal_warp = call_gdal_warp,
      compute_with_py_env = compute_with_py_env,
      detect_byte_bands = detect_byte_bands,
      plan_assets = plan_assets,
      tr = tr,
      resampling = resampling,
      t_srs = t_srs,
      te = te,
      lazy = lazy,
      w_cl_arg = w_cl_arg,
      config_options = x$config_options,
      quiet = quiet,
      src_df_warper = src_df_warper
    )
  )

  build_vrt_collection(
    warped_blocks,
    pixfun = NULL,
    maskfun = NULL,
    warped = TRUE
  )
}


#' Detect byte bands in a raster source
#' @param src Path to a raster file
#' @return Integer vector of band indices that are byte type
#' @keywords internal
#' @noRd
detect_byte_bands <- function(src) {
  ds <- methods::new(gdalraster::GDALRaster, src, read_only = TRUE)
  on.exit(ds$close(), add = TRUE)

  n_bands <- ds$getRasterCount()

  byte_idx <- purrr::map_lgl(seq_len(n_bands), function(b) {
    ds$getDataTypeName(b) == "Byte"
  })

  which(byte_idx)
}


warp_setup <- function(
  tr,
  resampling,
  t_srs,
  te,
  x,
  virtual,
  item_idx,
  byte_band_idx = NULL
) {
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
            "!" = "The numeric band ID for the image mask ({.val {mas_band_idx}}) does not exist."
          )
        )
      }
    }
  } else {
    mas_band_idx <- NULL
  }

  # Force nearest neighbor resampling for byte bands (e.g., masks, classification)
  if (is.null(byte_band_idx)) {
    byte_band_idx <- detect_byte_bands(x$vrt_src)
  }

  resamp_methods <- rep(resampling, length(assets))
  resamp_methods[mas_band_idx] <- "near" # mask band should be nearest neighbour
  resamp_methods[byte_band_idx] <- "near" # byte bands should be nearest neighbour

  of <- fs::file_temp(
    pattern = glue::glue("warpfile_{seq_along(assets)}_"),
    tmp_dir = getOption("vrt.cache"),
    ext = if (virtual) "vrt" else "tif"
  )

  n_assets <- length(assets)

  if (is.null(x$bbox) || is.null(x$srs)) {
    gdr <- methods::new(gdalraster::GDALRaster, x$vrt_src, read_only = TRUE)
    on.exit(gdr$close(), add = TRUE)
    x$bbox <- gdr$bbox()
    x$srs <- gdr$getProjection()
  }

  projwin <- glue::glue_collapse(
    c(x$bbox[1], x$bbox[4], x$bbox[3], x$bbox[2]),
    sep = ","
  )
  projwin_srs <- x$srs

  data.frame(
    band = seq_along(assets),
    resampling = resamp_methods,
    src = rep(x$vrt_src, n_assets),
    tsrs = rep(t_srs, n_assets),
    te = I(rep(list(as.numeric(te)), n_assets)),
    tr = I(rep(list(as.numeric(tr)), n_assets)),
    outtf = of,
    item_idx = item_idx,
    projwin = rep(projwin, n_assets),
    projwin_srs = rep(projwin_srs, n_assets)
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
      function(
        band,
        resampling,
        src,
        tsrs,
        te,
        tr,
        outtf,
        item_idx,
        projwin,
        projwin_srs
      ) {
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

        projwin_src <- glue::glue(
          "vrt://{src}?projwin={projwin}&projwin_srs={projwin_srs}"
        )

        compute_with_py_env(
          call_gdal_warp(
            projwin_src,
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
