# download_eopfzarr_driver <- function(out_dir = ".") {
#   releases <- gh::gh(
#     "GET /repos/EOPF-Sample-Service/GDAL-ZARR-EOPF/releases"
#   )
#   latest_release <- releases[[1]][["tag_name"]]

#   os <- tolower(Sys.info()[["sysname"]])
#   rlang::arg_match(os, c("linux", "darwin", "windows"))
#   os <- switch(os, "linux" = "Linux", "darwin" = "macOS", "windows" = "Windows")
#   download_url <- sprintf(
#     "https://github.com/EOPF-Sample-Service/GDAL-ZARR-EOPF/releases/download/%s/gdal_EOPFZarr-%s.zip",
#     latest_release,
#     os
#   )

#   tempd <- fs::dir_create(fs::path(tempdir(), "eopfzarr-temp"))

#   download.file(download_url, fs::path(tempd, "GDAL-ZARR-EOPF.zip"))

#   utils::unzip(fs::path(tempd, "GDAL-ZARR-EOPF.zip"), exdir = tempd)

#   dest_dir <- fs::path(tempd, sprintf("artifacts/gdal_EOPFZarr-%s", os))

#   if (!fs::dir_exists(dest_dir)) {
#     cli::cli_abort(
#       "Failed to find the extracted GDAL EOPF ZARR driver directory."
#     )
#   }

#   install_file <- fs::dir_ls(dest_dir, recurse = TRUE)

#   if (length(install_file) != 1) {
#     cli::cli_abort(
#       "{length(install_file)} files found in the extracted directory, expected exactly 1."
#     )
#   }

#   loc <- fs::file_copy(
#     install_file,
#     ".",
#     overwrite = TRUE
#   )

#   cli::cli_alert_success(
#     "GDAL EOPF ZARR driver downloaded successfully successfully to {loc}."
#   )

#   plugindir <- fs::path(system("gdal-config --plugindir", intern = TRUE))
#   cli::cli_alert_info(
#     "To install the driver you must copy {loc} to {plugindir}."
#   )

#   return(invisible(loc))
# }
