#' Save a vrt_block object to disk
#' @param x A `vrt_stack` or `vrt_block` object.
#' @param outfile A character string of the output file path. Must have a
#'   `.vrt` extension. Defaults to a temporary file inside
#'   `getOption("vrt.cache")`.
#' @param bundle Logical. If `TRUE`, copy every intermediate VRT in the
#'   dependency tree into the directory containing `outfile` and rewrite all
#'   `<SourceFilename>` paths relative to that directory. The result is a
#'   self-contained bundle that survives wiping the VRT cache. Remote sources
#'   (URLs, `/vsicurl/`, ...) cannot be bundled and are left untouched.
#' @param include_rasters Logical. Only meaningful when `bundle = TRUE`. If
#'   `TRUE`, copy local raster leaves (e.g. GeoTIFFs) into the bundle as well,
#'   producing a fully portable directory. Remote raster sources are skipped
#'   and surfaced as a single warning.
#' @return Invisibly, the normalised absolute path to the saved `.vrt` file.
#' @export
vrt_save <- function(
  x,
  outfile = fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt"),
  bundle = FALSE,
  include_rasters = FALSE
) {
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

#' @keywords internal
#' @noRd
#' @export
vrt_save.vrt_collection <- function(x, ...) {
  cli::cli_abort(
    c(
      "!" = "{.fn vrt_save} is not supported for {.cls vrt_collection} objects.",
      "i" = "A vrt_collection holds many per-item VRTs and is an intermediate state, not a saveable end product.",
      ">" = "Call {.fn vrt_stack} to combine the items into a single VRT, then {.fn vrt_save}, or {.fn vrt_compute} to materialise to disk."
    ),
    class = "vrtility_type_error"
  )
}

#' @export
vrt_save.vrt_block <- function(
  x,
  outfile = fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt"),
  bundle = FALSE,
  include_rasters = FALSE
) {
  if (fs::path_ext(outfile) != "vrt") {
    cli::cli_abort("The `outfile` extension must be'.vrt'.")
  }
  if (include_rasters && !bundle) {
    cli::cli_abort(
      c(
        "!" = "{.arg include_rasters} requires {.code bundle = TRUE}.",
        "i" = "Set {.code bundle = TRUE} to copy raster sources into a self-contained directory."
      )
    )
  }

  vrt_xml <- xml2::read_xml(x$vrt)

  if (bundle) {
    return(invisible(write_vrt_bundle(
      vrt_xml,
      outfile,
      cache_dir = getOption("vrt.cache"),
      include_rasters = include_rasters
    )))
  }

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
