#' Generate a Sentinel 2 VRT from a STAC query
#' @param bbox A numeric vector of the bounding box (length 4) in lat/long
#' @param start_date A character string of the start date
#' @param end_date A character string of the end date
#' @param assets A character vector of the asset names to include
#' @param max_cloud_cover A numeric value of the maximum cloud cover percentage
#' @param stac_source A character string of the STAC source
#' @param collection A character string of the collection to query
#' @return A stac_vrt object
#' @export
sentinel2_stac_vrt <- function(
    bbox,
    start_date,
    end_date,
    assets = c(
      "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A",
      "B09", "B11", "B12", "SCL"
    ),
    max_cloud_cover = 10,
    stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
    collection = "sentinel-2-l2a") {
  stac_its <- stac_query(
    bbox = bbox,
    stac_source = stac_source,
    start_date = start_date,
    end_date = end_date,
    collection = collection
  )

  if (!is.null(max_cloud_cover)) {
    stac_its <- stac_its |>
      rstac::items_filter(
        filter_fn = \(x) x$properties$`eo:cloud_cover` < max_cloud_cover
      )
  }
  stac_its <- sign_planetary_computer(stac_its)


  asset_vrts <- purrr::map(assets, function(x) {
    its_asset <- rstac::assets_select(stac_its, asset_names = x)

    urls <- suppressWarnings(
      rstac::assets_url(its_asset, append_gdalvsi = TRUE)
    )
    #
    urls
  }) |>
    purrr::set_names(assets) |>
    purrr::transpose() |>
    purrr::map(
      function(x) {
        tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

        gdalraster::buildVRT(tf, unlist(x),
          cl_arg = c(
            "-separate"
          ),
          quiet = TRUE
        )
        return(tf)
      }
    )

  master_vrt <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

  gdalraster::buildVRT(
    vrt_filename = master_vrt,
    input_rasters = unlist(asset_vrts),
    cl_arg = c(
      "-vrtnodata", "0"
    ),
    quiet = TRUE
  )

  mvrt <- xml2::read_xml(master_vrt)
  mvrt_bands <- xml2::xml_find_all(mvrt, "//VRTRasterBand")
  purrr::walk2(
    mvrt_bands,
    assets,
    function(x, y) {
      xml2::xml_set_attr(x, "Description", y)
    }
  )


  return(
    build_stac_vrt(
      xml = mvrt,
      bbox = bbox,
      start_date = start_date,
      end_date = end_date,
      n_its = rstac::items_length(stac_its),
      assets = assets
    )
  )
}
