unique_fp <- function(basepath, uid) {
  dir <- fs::path_dir(basepath)
  basefile <- fs::path_ext_remove(fs::path_file(basepath))

  if (nchar(uid) > 0) {
    uid <- gsub(
      ":",
      "-",
      gsub(" ", "_", strftime(lubridate::as_datetime(uid)))
    )
  }

  fs::path(
    dir,
    paste0(basefile, "_", uid),
    ext = "tif"
  )
}
