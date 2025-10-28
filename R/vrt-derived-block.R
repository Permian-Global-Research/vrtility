#' Create vrt blocks with derived bands.
#' @param x a `vrt_block` or `vrt_collection` object.
#' @param ... named R formulas for new bands to create. Formula variable names
#' must the names of existing bands in the vrt_block object. the argument name
#' is used to name the new (derived) band.
#'
#' @description
#' Create new vrt blocks containing derived expression bands.
#' @export
#' @examplesIf check_muparser()
#' s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#'
#' ex_collect <- vrt_collect(s2files[1])
#'
#' ex_ndvi <- vrt_derived_block(
#'   ex_collect,
#'   ndvi ~ (B08 - B04) / (B08 + B04)
#' )
#' plot(ex_ndvi)
#'
vrt_derived_block <- function(x, ...) {
  UseMethod("vrt_derived_block")
}

#' @noRd
#' @keywords internal
#' @export
vrt_derived_block.default <- function(x, ...) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_derived_block()')}
    not implemented for class {class(x)[1]}"
  )
}

#' @noRd
#' @keywords internal
#' @export
vrt_derived_block.vrt_stack <- function(x, ...) {
  NextMethod()
}

#' @export
vrt_derived_block.vrt_block <- function(x, ...) {
  v_assert_muparser()
  new_bands <- rlang::dots_list(...)

  purrr::iwalk(
    new_bands,
    ~ v_assert_type(
      .x,
      .y,
      "formula",
      nullok = FALSE
    )
  )

  new_bands <- v_assert_formula_valid(new_bands)

  vx <- xml2::read_xml(x$vrt)
  bands <- xml2::xml_find_all(vx, ".//VRTRasterBand")

  dband_reqs <- purrr::map(new_bands, function(b) {
    rqbn <- all.vars(rlang::f_rhs(b))

    # get bands with descriptions matching req_bands
    band_descs <- xml2::xml_text(xml2::xml_find_first(
      bands,
      ".//Description"
    ))

    if (any(!rqbn %in% band_descs)) {
      cli::cli_abort(
        "Band {cli::col_yellow(rqbn[!rqbn %in% band_descs])} not found"
      )
    }

    req_band_idx <- which(band_descs %in% rqbn)

    req_bands <- bands[req_band_idx]

    formula_scale_adj <- apply_band_formula_scales(req_bands, rqbn, b)

    band_srcs <- vrt_find_first_src(req_bands)
    return(list(
      req_band_idx = req_band_idx,
      band_srcs = band_srcs,
      muparser_exp = formula_scale_adj
    ))
  })

  xml2::xml_remove(bands)

  purrr::walk(
    dband_reqs,
    function(.x) {
      newband <- bands[[.x$req_band_idx[1]]]
      purrr::walk(.x$band_srcs[-1], ~ xml2::xml_add_child(newband, .x))
      xml2::xml_add_child(vx, newband, .where = xml2::xml_length(vx) - 1)
    }
  )

  dbands <- xml2::xml_find_all(vx, ".//VRTRasterBand")

  muparser_exprs <- purrr::map_chr(dband_reqs, "muparser_exp")

  purrr::pwalk(
    list(
      band = dbands,
      band_name = names(new_bands),
      idx = seq_along(dbands),
      mu_exp = muparser_exprs
    ),
    function(band, band_name, idx, mu_exp) {
      xml2::xml_set_attr(band, "band", idx)
      xml2::xml_set_attr(band, "dataType", "Float32")
      desc <- xml2::xml_find_first(band, ".//Description")
      if (is.na(desc)) {
        desc <- xml2::xml_add_child(band, "Description")
      }
      xml2::xml_set_text(desc, band_name)

      set_gdal_pixfun_xml(band, "expression", list(expression = mu_exp))
    }
  )
  # browser()

  drop_scale(vx) # scale and offset are now applied at the formula level
  set_nodatavalue(
    vx,
    value = "NaN",
    nodata_targets = c(".//NoDataValue")
  )
  # Write back to block
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(vx, tf)

  expr_pf <- purrr::imap_chr(muparser_exprs, function(.x, .y) {
    paste0(.y, " ~ ", .x)
  })

  build_vrt_block(
    vrt_to_vrt(tf),
    maskfun = x$maskfun,
    pixfun = expr_pf,
    warped = x$warped
  )
}


#' @export
vrt_derived_block.vrt_collection <- function(x, ...) {
  dots <- rlang::dots_list(...)
  block_list <- purrr::map(
    x[[1]],
    ~ do.call(vrt_derived_block, c(list(.x), dots))
  )
  build_vrt_collection(
    block_list,
    pixfun = block_list[[1]]$pixfun,
    maskfun = x$maskfun,
    warped = x$warped
  )
}


apply_band_formula_scales <- function(req_bands, req_band_names, form) {
  orig_exp <- gsub(
    "\\s+",
    " ",
    paste(deparse(rlang::f_rhs(form)), collapse = "")
  )

  sc_off <- purrr::map(c(".//Scale", ".//Offset"), function(v) {
    scale_node <- xml2::xml_find_all(req_bands, v)
    # read scale_node as numeric
    format(xml2::xml_double(scale_node), scientific = FALSE)
  }) |>
    purrr::set_names(c("scale", "offset")) |>
    purrr::list_transpose()

  if (length(sc_off) != 0) {
    sc_off <- purrr::set_names(sc_off, req_band_names)
  }

  if (all(is.null(unlist(sc_off)))) {
    return(orig_exp)
  }

  remapped_vars <- purrr::imap(sc_off, function(.x, .y) {
    scale_chr <- if (!is.null(.x[["scale"]])) {
      paste0(" * ", .x[["scale"]])
    } else {
      ""
    }

    offset_chr <- if (!is.null(.x[["offset"]])) {
      paste0(" + ", .x[["offset"]])
    } else {
      ""
    }

    paste0("(", .y, scale_chr, offset_chr, ")")
  })

  for (i in seq_along(remapped_vars)) {
    orig_exp <- gsub(
      paste0("\\b", names(remapped_vars)[i], "\\b"),
      remapped_vars[[i]],
      orig_exp
    )
  }

  return(orig_exp)
}
