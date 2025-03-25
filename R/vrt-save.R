#' Save a vrt_block object to disk
#' @param x A `vrt_stack` of `vrt_block` object.
#' @param outfile A character string of the output file
#' @export
vrt_save <- function(x, outfile) {
  UseMethod("vrt_save")
}

#' @keywords internal
#' @noRd
vrt_save.default <- function(x, ...) {
  cli::cli_abort(
    "The vrt_save method is not implemented for class {class(x)}",
    class = "vrtility_type_error"
  )
}

#' @export
vrt_save.vrt_block <- function(
  x,
  outfile = fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
) {
  if (fs::path_ext(outfile) != "vrt") {
    cli::cli_abort("The `outfile` extension must be'.vrt'.")
  }

  vrt_xml <- xml2::read_xml(x$vrt)

  if (fs::path_has_parent(outfile, getOption("vrt.cache"))) {
    xml2::write_xml(vrt_xml, outfile)
  } else {
    vrt_xml <- xml2::read_xml(x$vrt)
    tempvrt <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
    xml2::write_xml(
      vrt_xml,
      tempvrt
    )

    gdalraster::buildVRT(outfile, tempvrt, quiet = TRUE)
  }

  invisible(normalizePath(outfile))
}

# #' Save a vrt_block object to disk
# #' @param x A `vrt_stack` of `vrt_block` object.
# #' @param outfile A character string of the output file
# #' @param bands A numeric vector of band numbers to include in the output. The
# #' default is `NULL`, which includes all bands.
# #' @export
# vrt_save <- function(x, outfile) {
#   UseMethod("vrt_save")
# }

# #' @keywords internal
# #' @noRd
# vrt_save.default <- function(x, ...) {
#   cli::cli_abort(
#     "The vrt_save method is not implemented for class {class(x)}",
#     class = "vrtility_type_error"
#   )
# }

# #' @export
# vrt_save.vrt_block <- function(
#   x,
#   outfile = fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt"),
#   bands = NULL
# ) {
#   if (fs::path_ext(outfile) != "vrt") {
#     cli::cli_abort("The `outfile` extension must be'.vrt'.")
#   }
#   v_assert_type(bands, "bands", "numeric", nullok = TRUE)

#   vrt_xml <- xml2::read_xml(x$vrt)
#   tempvrt <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
#   xml2::write_xml(
#     vrt_xml,
#     tempvrt
#   )

#   cl_arg <- if (!is.null(bands)) {
#     paste0("-b ", paste(bands, collapse = ","))
#   } else {
#     NULL
#   }

#   gdalraster::buildVRT(outfile, tempvrt, cl_arg = cl_arg, quiet = TRUE)

#   invisible(normalizePath(outfile))
# }
