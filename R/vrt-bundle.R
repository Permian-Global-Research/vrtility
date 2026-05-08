#' Bundle a saved VRT and all its dependencies into a self-contained directory
#'
#' Walks the VRT dependency tree, copies every intermediate VRT (and optionally
#' the underlying local rasters) into the directory containing `outfile`, and
#' rewrites all `<SourceFilename>` paths to be relative to that directory.
#' Remote sources (URLs, /vsicurl/, /vsis3/, ...) are detected via
#' [assert_is_url()] and left in place: they cannot be bundled.
#'
#' @param vrt_xml An `xml_document` of the root VRT (already in memory).
#' @param outfile The destination path for the root VRT. Its parent directory
#'   becomes the bundle root.
#' @param cache_dir The cache directory used to resolve relative paths in the
#'   root VRT (typically `getOption("vrt.cache")`).
#' @param include_rasters If `TRUE`, copy local raster sources (e.g. GeoTIFFs)
#'   into the bundle as well. Remote sources are skipped with a single warning.
#' @return The absolute path to the bundled root VRT (invisibly).
#' @noRd
#' @keywords internal
write_vrt_bundle <- function(
  vrt_xml,
  outfile,
  cache_dir,
  include_rasters = FALSE
) {
  outfile <- fs::path_abs(outfile)
  bundle_dir <- fs::path_dir(outfile)
  fs::dir_create(bundle_dir)

  # registry: map original absolute source path -> bundled basename
  registry <- new.env(parent = emptyenv())
  # processed: bundled VRTs already walked (prevents re-rewriting + spurious
  # warnings when the same intermediate VRT is referenced via multiple bands)
  processed <- new.env(parent = emptyenv())
  # remote_sources: URLs encountered while include_rasters = TRUE; surfaced as
  # a single end-of-walk warning
  remote_sources <- new.env(parent = emptyenv())

  # seed the registry with the user-chosen outfile name so we don't clobber it
  assign(outfile, fs::path_file(outfile), envir = registry)

  bundled_name <- function(src_abs) {
    if (exists(src_abs, envir = registry, inherits = FALSE)) {
      return(get(src_abs, envir = registry))
    }
    base <- fs::path_file(src_abs)
    candidate <- base
    used <- purrr::map_chr(ls(registry), ~ get(.x, envir = registry))
    i <- 1L
    while (
      candidate %in% used ||
        fs::file_exists(fs::path(bundle_dir, candidate))
    ) {
      candidate <- paste0(
        fs::path_ext_remove(base),
        "_",
        i,
        ".",
        fs::path_ext(base)
      )
      i <- i + 1L
    }
    assign(src_abs, candidate, envir = registry)
    candidate
  }

  process <- function(vrt_in_bundle, original_dir) {
    if (exists(vrt_in_bundle, envir = processed, inherits = FALSE)) {
      return(invisible())
    }
    assign(vrt_in_bundle, TRUE, envir = processed)

    doc <- xml2::read_xml(vrt_in_bundle)
    src_nodes <- xml2::xml_find_all(doc, ".//SourceFilename")

    purrr::walk(src_nodes, function(node) {
      raw <- xml2::xml_text(node)

      # Remote sources cannot be bundled; leave them in place.
      if (assert_is_url(raw)) {
        if (include_rasters) {
          assign(raw, TRUE, envir = remote_sources)
        }
        return(invisible())
      }

      rel_attr <- xml2::xml_attr(node, "relativeToVRT")
      is_relative <- !is.na(rel_attr) && rel_attr == "1"

      src_abs <- if (is_relative) {
        fs::path_abs(fs::path(original_dir, raw))
      } else {
        fs::path_abs(raw)
      }

      if (!fs::file_exists(src_abs)) {
        cli::cli_warn(
          "Skipping missing source {.path {src_abs}}."
        )
        return(invisible())
      }

      is_vrt <- tolower(fs::path_ext(src_abs)) == "vrt"

      if (is_vrt || include_rasters) {
        new_name <- bundled_name(src_abs)
        new_path <- fs::path(bundle_dir, new_name)
        if (!fs::file_exists(new_path)) {
          fs::file_copy(src_abs, new_path)
        }
        xml2::xml_set_text(node, new_name)
        xml2::xml_set_attr(node, "relativeToVRT", "1")

        if (is_vrt) {
          # recurse using the *original* dir of this nested VRT so its
          # own relative paths resolve against the cache, not the bundle.
          process(new_path, fs::path_dir(src_abs))
        }
      } else {
        # local raster, include_rasters = FALSE: keep an absolute reference
        xml2::xml_set_text(node, src_abs)
        xml2::xml_set_attr(node, "relativeToVRT", "0")
      }
    })

    xml2::write_xml(doc, vrt_in_bundle)
  }

  # write the root VRT to its destination, then walk it
  xml2::write_xml(vrt_xml, outfile)
  process(outfile, cache_dir)

  if (include_rasters && length(ls(remote_sources)) > 0) {
    remotes <- ls(remote_sources)
    cli::cli_warn(c(
      "!" = paste0(
        "{length(remotes)} remote source{?s} cannot be bundled and ",
        "{?was/were} left as URL{?s}:"
      ),
      purrr::set_names(remotes, rep("*", length(remotes)))
    ))
  }

  invisible(normalizePath(outfile))
}
