#' Save a vrt_block object to disk
#' @param x A `vrt_stack` of `vrt_block` object.
#' @param outfile A character string of the output file
#' @export
vrt_save <- function(x, outfile) {
  UseMethod("vrt_save")
}

#' @keywords internal
#' @noRd
#' @export
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
    all_files <- xml2::xml_find_all(vrt_xml, ".//SourceFilename")

    purrr::walk(all_files, function(x) {
      src_path <- xml2::xml_text(x)
      abs_path <- fs::path(getOption("vrt.cache"), src_path)
      if (!fs::file_exists(abs_path)) {
        cli::cli_abort(
          c(
            "!" = "The file {abs_path} does not exist."
          )
        )
      }
      xml2::xml_set_text(x, abs_path)
      rel_to_att <- xml2::xml_attr(x, "relativeToVRT")
      if (rel_to_att != "0") {
        xml2::xml_set_attr(x, "relativeToVRT", "0")
      }
    })

    xml2::write_xml(
      vrt_xml,
      outfile
    )
  }

  invisible(normalizePath(outfile))
}
