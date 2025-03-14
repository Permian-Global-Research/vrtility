#' Construct the base VRT object for composing VRT pipelines.
#' @param x An object to be used to create a vrt_x object see details.
#' @param band_descriptions A character vector of band descriptions.
#' @param datetimes A character vector of datetimes.
#' @return A vrt_collection object.
#' @rdname vrt_collect
#' @export
#' @details For now the main way to create a vrt_collection object, which forms
#' the basis of the vrrt-based pipelines in vrtility is using a doc_items
#' object from the `rstac` package. For more info on how to create a doc_items
#' object see [sentinel2_stac_query()]. To build a vrt_stack object a
#' vrt_collection is required first. The vrt_collection object is essentially a
#' list of VRT files. At this stage no alignment is carried out -  and the
#' rasters are virtualised as-is. In this state, we can apply masks fro example
#' and when summarisation is required we can use vrt_stack - however, in order
#' to create a stack the collection must xontain images from a single spatial
#' reference system (SRS). If there are mutliple SRS values, use `vrt_warp()`
#' to unify the projection of the collection.
vrt_collect <- function(
  x,
  band_descriptions,
  datetimes
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
vrt_collect.character <- function(
  x,
  band_descriptions = NULL,
  datetimes = rep("", length(x))
) {
  assert_files_exist(x)
  v_assert_type(
    band_descriptions,
    "band_descriptions",
    "character",
    nullok = TRUE
  )
  v_assert_type(datetimes, "datetimes", "character", nullok = TRUE)
  v_assert_length(datetimes, "datetimes", length(x), nullok = TRUE)

  vrt_items <- purrr::map2(unname(x), datetimes, function(.x, .y) {
    tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

    gdalraster::buildVRT(
      tf,
      .x,
      quiet = TRUE
    )

    ds <- methods::new(gdalraster::GDALRaster, .x)
    on.exit(ds$close())
    nbands <- ds$getRasterCount()
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
  ...
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
  warped = FALSE,
  ...
) {
  uniq_crs <- purrr::map(
    x,
    function(.x) .x$srs
  ) |>
    unlist() |>
    unique()

  if (length(uniq_crs) > 1) {
    bbox <- NA
  } else {
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
  }

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

  if (warped) {
    warped <- "vrt_collection_warped"
  } else {
    warped <- NULL
  }

  class(rvrt) <- c(warped, "vrt_collection", "vrt_block", "list")

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
