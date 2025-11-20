#' Construct the base VRT object for composing VRT pipelines.
#' @param x An object to be used to create a vrt_x object see details.
#' @param bands A numeric vector of band indices to include in the VRT
#' collection
#' @param band_descriptions A character vector of band descriptions.
#' @param datetimes A character vector of datetimes.
#' @param config_opts A named character vector of GDAL configuration options.
#' @param vsi_prefix A character string indicating the VSI prefix to use for
#' the VRT sources. Defaults to `"/vsicurl/"`.
#' See \code{\link[gdalraster]{vsi_get_fs_prefixes}} for available options.
#' @param driver A character string indicating the GDAL driver to use for the
#' input source(s). if "" is provided, the driver will be automatically determined by GDAL.
#' for available drivers use \code{\link{gdal_raster_drivers}}.
#' @param check_src A logical indicating whether to check that the source files
#' exist. Default is TRUE.
#' @return A vrt_collection object.
#' @rdname vrt_collect
#' @export
#' @details The main way to create a vrt_collection object, which forms
#' the basis of the vrrt-based pipelines in vrtility is using a doc_items
#' object from the `rstac` package. For more info on how to create a doc_items
#' object see [stac_query()]. To build a vrt_stack object a
#' vrt_collection is required first. The vrt_collection object is essentially a
#' list of VRT files. At this stage no alignment is carried out -  and the
#' rasters are virtualised as-is. In this state, we can apply masks, for example
#' and when summarisation is required we can use vrt_stack - however, in order
#' to create a stack the collection must contain images from a single spatial
#' reference system (SRS). If there are mutliple SRS values, use `vrt_warp()`
#' to unify the projection of the collection (This is almost always a good idea
#' anyway).
#'
#' We can also create a VRT collection from a set of files. This is useful when
#' we have the data on disk or as a downstream step after first processing a
#' stac collection.
#' @examples
#' s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#' vrt_collect(s2files)
#'
#' # we can also combine multiple vrt collections
#' c(vrt_collect(s2files[1:2]), vrt_collect(s2files[3:4]))
#'
#' @examplesIf interactive()
#' s2q <- sentinel2_stac_query(
#'  bbox = c(-12.386, -37.214, -12.186, -37.014),
#'  start_date = "2023-01-01",
#'  end_date = "2023-01-31",
#'  max_cloud_cover = 10,
#'  assets = c("B02", "B03", "B04", "B08", "SCL")
#' )
#'
#' vrt_collect(s2q)
#'
vrt_collect <- function(
  x,
  ...
) {
  UseMethod("vrt_collect")
}

#' @noRd
#' @keywords internal
#' @export
vrt_collect.default <- function(x, ...) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_collect()')}
    not implemented for class {class(x)[1]}"
  )
}


#' @rdname vrt_collect
#' @export
vrt_collect.character <- function(
  x,
  config_opts = gdal_config_opts(),
  bands = NULL,
  band_descriptions = NULL,
  datetimes = rep("", length(x)),
  vsi_prefix = "",
  driver = "",
  check_src = TRUE,
  ...
) {
  gdal_vrt_collect_arg_checks(vsi_prefix, driver, config_opts)
  if (check_src) {
    assert_files_exist(x, url_possible = TRUE)
  }
  x <- gdal_driver_vsi_src_builder(x, vsi_prefix, driver)
  v_assert_type(
    bands,
    "bands",
    c("numeric", "integer"),
    multiple = TRUE,
    nullok = TRUE
  )
  v_assert_type(
    band_descriptions,
    "band_descriptions",
    "character",
    nullok = TRUE
  )

  v_assert_type(datetimes, "datetimes", "character", nullok = TRUE)
  v_assert_length(datetimes, "datetimes", length(x), nullok = TRUE)

  orig_config <- set_gdal_config(config_opts)
  on.exit(set_gdal_config(orig_config))

  if (!is.null(bands)) {
    band_args <- c(rbind("-b", sort(bands)))
  } else {
    band_args <- NULL
  }
  vrt_items <- purrr::map2(unname(x), datetimes, function(.x, .y) {
    tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

    gdalraster::buildVRT(
      tf,
      .x,
      quiet = TRUE,
      cl_arg = c(
        band_args,
        src_block_size(.x)
      )
    )

    ds <- methods::new(gdalraster::GDALRaster, .x)
    on.exit(ds$close(), add = TRUE)
    dst <- methods::new(gdalraster::GDALRaster, tf, readonly = FALSE)
    on.exit(dst$close(), add = TRUE)

    nbands <- dst$getRasterCount()

    purrr::walk(seq_len(nbands), function(b) {
      src_scale <- ds$getScale(band = b)
      src_offset <- ds$getOffset(band = b)
      if (!is.na(src_scale)) {
        dst$setScale(band = b, scale = src_scale)
      }
      if (!is.na(src_offset)) {
        dst$setOffset(band = b, offset = src_offset)
      }
    })

    v_assert_length(band_descriptions, "band_descriptions", nbands)
    if (is.null(band_descriptions)) {
      band_descriptions <- purrr::map_chr(
        seq_len(nbands),
        ~ ds$getDescription(.x)
      )

      if (any(band_descriptions == "")) {
        cli::cli_warn(
          c(
            "!" = "Some/all bands do not have descriptions.",
            "i" = "Using generic band descriptions."
          )
        )
        band_descriptions <- paste0("band_", seq_len(nbands))
      }
    }

    # required because of subsequent reading below.
    if (dst$isOpen()) {
      dst$close()
    }

    tf <- set_vrt_descriptions(
      x = tf,
      band_descriptions,
      as_file = TRUE
    )

    tf <- set_vrt_metadata(
      tf,
      keys = "datetime",
      values = .y,
      as_file = TRUE
    )

    build_vrt_block(tf)
  })

  build_vrt_collection(
    vrt_items
  )
}

#' @rdname vrt_collect
#' @export
vrt_collect.doc_items <- function(
  x,
  config_opts = gdal_config_opts(),
  vsi_prefix = "",
  driver = "",
  ...
) {
  gdal_vrt_collect_arg_checks(vsi_prefix, driver, config_opts)
  daemon_setup(config_opts)
  orig_config <- set_gdal_config(config_opts)
  on.exit(set_gdal_config(orig_config))

  assets <- rstac::items_assets(x)[order(as.numeric(gsub(
    "\\D",
    "",
    rstac::items_assets(x)
  )))]

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

  vrt_items <- purrr::map(
    items_uri,
    purrr::in_parallel(
      function(item) {
        srcs <- purrr::map_chr(item, ~ .x$uri)
        dttm <- unique(purrr::map_chr(item, ~ .x$dttm))

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
        purrr::insistently(
          function() {
            gdalraster::buildVRT(
              tf,
              srcs,
              cl_arg = c(
                "-separate",
                src_block_size(srcs[1])
              ),
              quiet = TRUE
            )
          },
          rate = purrr::rate_backoff(
            pause_base = getOption("vrt.pause.base"),
            pause_cap = getOption("vrt.pause.cap"),
            max_times = getOption("vrt.max.times")
          )
        )()

        tf <- set_vrt_descriptions(
          x = tf,
          names(item),
          as_file = TRUE
        )

        tf <- set_vrt_metadata(
          tf,
          keys = "datetime",
          values = dttm,
          as_file = TRUE
        )

        build_vrt_block(tf)
      },
      set_vrt_descriptions = set_vrt_descriptions,
      set_vrt_metadata = set_vrt_metadata,
      build_vrt_block = build_vrt_block,
      src_block_size = src_block_size
    )
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
  warped = FALSE,
  ...
) {
  if (length(x) == 1) {
    # return as a vrt_block object if only one item
    return(x[[1]])
  }

  dateorder <- purrr::map_vec(x, ~ lubridate::as_datetime(.x$date_time)) |>
    order()

  x <- x[dateorder]

  # Extract all properties in a single pass to avoid multiple iterations
  uniq_crs <- unique(purrr::map_chr(x, function(.x) .x$srs))
  sd <- purrr::map_chr(x, function(.x) .x$date_time)
  uniq_assets <- unique(unlist(
    purrr::map(x, function(.x) .x$assets),
    use.names = FALSE
  ))
  mask_band_name <- unique(purrr::map_chr(x, function(.x) .x$mask_band_name))

  # Only compute bbox and res if needed
  if (length(uniq_crs) > 1) {
    bbox_all <- NA
    bbox <- NA
  } else {
    bbox_all <- purrr::map(x, function(.x) .x$bbox)
    bbox <- purrr::reduce(
      bbox_all,
      function(.x, .y) {
        c(
          min(.x[1], .y[1]),
          min(.x[2], .y[2]),
          max(.x[3], .y[3]),
          max(.x[4], .y[4])
        )
      }
    )
  }

  all_res <- purrr::map(x, function(.x) .x$res)
  min_res <- purrr::reduce(
    all_res,
    function(.x, .y) {
      c(
        min(.x[1], .y[1]),
        min(.x[2], .y[2])
      )
    }
  )

  # set to a warped class if all items have equal extent, res and crs.
  if (warped) {
    warp_class <- "vrt_collection_warped"
  } else {
    # Check uniqueness more efficiently
    uniq_bbox_count <- if (is.na(bbox_all[1])) 2 else length(unique(bbox_all))
    uniq_res_count <- length(unique(all_res))

    if (
      uniq_bbox_count == 1 &&
        length(uniq_crs) == 1 &&
        uniq_res_count == 1
    ) {
      warp_class <- "vrt_collection_warped"
      warped <- TRUE
    } else {
      warp_class <- NULL
    }
  }

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
    maskfun = maskfun,
    warped = warped
  )

  class(rvrt) <- c(warp_class, "vrt_collection", "vrt_block", "list")

  return(rvrt)
}


#' @export
#' @param xml logical indicating whether to print the XML of the VRT collection.
#' @param pixfun logical indicating whether to print the pixel function.
#' @param maskfun logical indicating whether to print the mask function.
#' @param blocks A logical indicating whether to print the blocks instead of
#' the collection summary.
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

#' @export
#' @rdname vrt_collect
#' @param ... In the case of `c`, additional vrt_collection objects to
#' concatenate to `x`. Otherwise, additional arguments to pass to the method or
#' unused.
#' @details
#' You can use the `c` method to combine multiple vrt_collection objects. All
#' collections must have the same number of bands.
c.vrt_block <- function(x, ...) {
  dots <- rlang::dots_list(...)
  nbands <- length(x$assets)

  purrr::walk(dots, function(vrtc) {
    v_assert_type(
      vrtc,
      "...",
      c("vrt_collection", "vrt_block"),
      nullok = FALSE,
      multiple = TRUE
    )
    v_assert_length(vrtc$assets, "...", nbands)
  })

  dots_inherit_block <- purrr::map_lgl(dots, ~ class(.x)[1] == "vrt_block")

  flat_blocks <- dots[dots_inherit_block]
  flatten_collects <- purrr::flatten(purrr::map(
    dots[!dots_inherit_block],
    function(x) {
      x[[1]]
    }
  ))

  flatten_dots <- c(flat_blocks, flatten_collects)

  x_set <- if (inherits(x, "vrt_collection")) {
    x[[1]]
  } else {
    list(x)
  }

  clist <- (c(x_set, flatten_dots))

  unique_attr <- function(at) {
    unlist(unique(x[at], purrr::flatten(purrr::map(dots, function(x) x[at]))))
  }

  build_vrt_collection(
    clist,
    pixfun = unique_attr("pixfun"),
    maskfun = unique_attr("maskfun"),
    warped = all(unique_attr("warped")),
  )
}

#' @export
#' @rdname vrt_collect
c.vrt_collection <- function(x, ...) {
  NextMethod()
}

#' internal gdal vrt_collect argument checks
#' @noRd
#' @keywords internal
gdal_vrt_collect_arg_checks <- function(vsi_prefix, driver, config_opts) {
  rlang::arg_match(
    vsi_prefix,
    c("", gdalraster::vsi_get_fs_prefixes()),
    multiple = TRUE
  )
  rlang::arg_match(driver, c("", gdal_raster_drivers(TRUE)))

  if (driver == "EOPFZARR") {
    check_blosc()
  }

  v_assert_is_named(config_opts, "config_opts")
  v_assert_type(config_opts, "config_opts", "character", multiple = TRUE)
}
