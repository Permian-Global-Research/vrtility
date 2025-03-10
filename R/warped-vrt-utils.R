#' Create a warped VRT from a source raster
#' @param src character path to the source raster
#' @param t_srs character target SRS
#' @return character path to the warped VRT
#' @keywords internal
#' @noRd
#' @details A wrapper to simplify the process of creating a warped VRT from a
#' source raster. functionality here is deliberately limited. could expand if
#' needed. Issue we have is with srcs with different srs. This function enables
#' us to transform the srs right at the start of the vrt blocking process.
vrt_to_warped_vrt <- function(
  src,
  band,
  t_srs,
  te,
  tr = NULL,
  resampling = "bilinear"
) {
  tfw <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

  call_vrt_compute(
    src,
    tfw,
    t_srs,
    warp_options = combine_warp_opts(
      c(
        "-b",
        band,
        "-r",
        resampling
      ),
      te,
      tr
    ),
    config_options = getOption("vrt.gdal.config.options"),
    quiet = TRUE
  )
}
