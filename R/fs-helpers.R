unique_fp <- function(uid, basepath) {
  dir <- fs::path_dir(basepath)
  basefile <- fs::path_ext_remove(fs::path_file(basepath))

  uid <- gsub(
    ":",
    "-",
    gsub(" ", "_", strftime(lubridate::as_datetime(uid)))
  ) |>
    make.unique(sep = "_")

  fs::path(
    dir,
    paste0(basefile, "_", uid),
    ext = "tif"
  )
}
