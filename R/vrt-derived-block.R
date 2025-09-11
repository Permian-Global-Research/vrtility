#' Create vrt blocks with derived bands.
#' @param ... named R formulas for new bands to create. Formula variable names
#' must the names of existing bands in the vrt_block object. the argument name
#' is used to name the new (derived) band.
#'
#' @description
#' Create new vrt blocks containing derived expression bands.
#' @export
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
  cli::cli_abort(
    "{cli::code_highlight('vrt_derived_block()')}
    cannot be used with a vrt_stack object.
    Each band must contain a single source raster."
  )
}

#' @export
vrt_derived_block.vrt_block <- function(x, ...) {
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

  muparser_exprs <- purrr::map(
    new_bands,
    ~ {
      gsub("\\s+", " ", paste(deparse(rlang::f_rhs(.x)), collapse = ""))
    }
  )

  vx <- xml2::read_xml(x$vrt)
  bands <- xml2::xml_find_all(vx, ".//VRTRasterBand")

  dband_reqs <- purrr::map(new_bands, function(b) {
    rqbn <- all.vars(b)

    # get bands with descriptions matching req_bands
    band_descs <- xml2::xml_text(xml2::xml_find_first(
      bands,
      ".//Description"
    ))
    req_band_idx <- which(band_descs %in% rqbn)
    req_bands <- bands[req_band_idx]
    assert_matching_scales(req_bands, rqbn)
    band_srcs <- vrt_find_first_src(req_bands)
    return(list(
      req_band_idx = req_band_idx,
      band_srcs = band_srcs
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

  purrr::pwalk(
    list(
      band = dbands,
      band_name = names(new_bands),
      idx = seq_along(dbands),
      mu_exp = muparser_exprs
    ),
    function(band, band_name, idx, mu_exp) {
      xml2::xml_set_attr(band, "band", idx)
      desc <- xml2::xml_find_first(band, ".//Description")
      if (is.na(desc)) {
        desc <- xml2::xml_add_child(band, "Description")
      }
      xml2::xml_set_text(desc, band_name)

      set_gdal_pixfun_xml(band, "expression", list(expression = mu_exp))
    }
  )

  # Write back to block
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(vx, tf)

  build_vrt_block(
    tf,
    maskfun = x$maskfun,
    pixfun = muparser_exprs,
    warped = x$warped
  )
}


assert_matching_scales <- function(req_bands, req_band_names) {
  sc_off <- purrr::map(c(".//Scale", ".//Offset"), function(v) {
    scale_node <- xml2::xml_find_all(req_bands, v)
    # read scale_node as numeric
    xml2::xml_double(scale_node)
  })

  purrr::walk(sc_off, function(v) {
    if (!all(sapply(v, function(x) isTRUE(all.equal(x, v[1]))))) {
      cli::cli_abort(
        c(
          "x" = "All source bands must have the same scale and offset values.",
          "i" = "Band{?s}: {req_band_names} have a scale/offset mismatch."
        )
      )
    }
  })
  invisible()
}
