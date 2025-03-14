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
  xml2::write_xml(vrt_xml, outfile)
  invisible(outfile)
}
