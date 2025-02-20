#' Compose a standard VRT for compositing at the band level
#' @param src_files A character vector of file paths to the source rasters
#' @param vrt_opts A list of options to pass to the VRT
#' @param pix_fun A character string of the pixel function code
#' @return A character string of the path to the modified VRT
#' @keywords internal
#' @noRd
vrt_pixfun <- function(src_files, vrt_opts, pix_fun) {
  # print("here")
  # browser()
  band_names <- xml2::read_xml(src_files[1]) |>
    xml2::xml_find_all("//VRTRasterBand") |>
    xml2::xml_attr("Description")

  tvrt <- xml2::read_xml(src_files)

  # Modify the first band to add pixel function
  bands <- xml2::xml_find_all(tvrt, "//VRTRasterBand")

  purrr::walk2(bands, band_names, function(x, y) {
    xml2::xml_set_attr(x, "subClass", "VRTDerivedRasterBand")
    xml2::xml_set_attr(x, "Description", y)

    # Add pixel function elements
    xml2::xml_add_child(x, "PixelFunctionType", "median")
    xml2::xml_add_child(x, "PixelFunctionLanguage", "Python")

    # Add pixel function code
    pixel_func_code <- xml2::xml_add_child(x, "PixelFunctionCode")
    # Add pixel function code as CDATA
    cdata_node <- xml2::xml_cdata(pix_fun)
    xml2::xml_add_child(pixel_func_code, cdata_node)
  })

  # browser()

  # Write modified VRT
  out_vrt <- fs::file_temp(ext = "vrt")
  xml2::write_xml(tvrt, out_vrt)
  return(out_vrt)
}


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


  # browser()
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
        tf <- fs::file_temp(ext = "vrt")

        gdalraster::buildVRT(tf, unlist(x),
          cl_arg = c(
            "-separate"
          ),
          quiet = TRUE
        )
        return(tf)
      }
    )

  master_vrt <- fs::file_temp(ext = "vrt")
  # browser()
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

  rvrt <- list(
    vrt = as.character(mvrt),
    bbox = bbox,
    start_date = start_date,
    end_date = end_date,
    n_items = rstac::items_length(stac_its),
    assets = assets
  )

  class(rvrt) <- "stac_vrt"

  return(rvrt)
}

#' Print a stac_vrt object
#' @param x A stac_vrt object
#' @param xml A logical indicating whether to print the XML
#' @param ... Additional arguments not used
#' @export
print.stac_vrt <- function(x, xml = FALSE, ...) {
  cli::cli_inform(c(">" = cli::style_bold(cli::col_green("STAC VRT"))))
  if (xml) {
    xml_printer(x$vrt)
  } else {
    cli::cli_inform(
      c(
        paste(cli::style_bold(
          cli::col_yellow("VRT XML: "),
          "[hidden]"
        )),
        " " = cli::style_italic("run {cli::code_highlight('print(x, xml = TRUE)')} to view")
      )
    )
  }
  cli::cli_inform(
    c(
      paste(
        cli::style_bold(cli::col_blue("Bounding Box:")),
        paste(x$bbox, collapse = " ")
      ),
      paste(cli::style_bold(cli::col_blue("Start Date:")), x$start_date),
      paste(cli::style_bold(cli::col_blue("End Date:")), x$end_date),
      paste(cli::style_bold(cli::col_blue("Number of Items:")), x$n_items),
      paste(
        cli::style_bold(cli::col_blue("Assets:")),
        paste(x$assets, collapse = ", ")
      )
    )
  )
  invisible(x)
}

xml_printer <- function(x) {
  cli::style_bold("XML process graph: \n\n") |>
    cli::col_yellow() |>
    cat()
  paste0(x, "\n") |>
    cli::style_italic() |>
    cli::col_green() |>
    cat()
}
