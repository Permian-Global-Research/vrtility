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
