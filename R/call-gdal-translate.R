#' A very thin wrapper around the `gdalraster::translate` function
#' @keywords internal
#' @noRd
call_gdal_tanslate <- function(
  src_files,
  outfile,
  cl_arg,
  config_options,
  quiet = TRUE
) {
  v_assert_true(fs::file_exists(src_files), "src_files")

  orig_config <- set_gdal_config(config_options)
  on.exit(set_gdal_config(orig_config))

  gdalraster::translate(
    src_files,
    outfile,
    cl_arg = cl_arg,
    quiet = quiet
  )

  return(outfile)
}
