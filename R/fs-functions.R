#' @noRd
#' @keywords internal
unique_fp <- function(uid, basepath) {
  dir <- fs::path_dir(basepath)
  basefile <- fs::path_ext_remove(fs::path_file(basepath))

  uid <- suppressWarnings(gsub(
    ":",
    "-",
    gsub(" ", "_", strftime(lubridate::as_datetime(uid)))
  ))

  uid[is.na(uid)] <- ""

  uid <- make.unique(uid, sep = "_")

  fs::path(
    dir,
    paste0(basefile, "_", uid),
    ext = "tif"
  )
}

#' Extract the actual file path from a VRT connection string.
#' @param path A VRT file path, which may be a connection string starting with "vrt://"
#' @return The extracted file path from the VRT connection string, or the original path if it does not start with "vrt://"
#' @keywords internal
#' @noRd
extract_vrt_path <- function(path) {
  if (grepl("^vrt://", path)) {
    # Remove 'vrt://' prefix and query string
    sub("\\?.*$", "", sub("^vrt://", "", path))
  } else {
    path
  }
}
