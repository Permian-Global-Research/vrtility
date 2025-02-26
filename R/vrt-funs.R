#' Compose a standard VRT for compositing at the band level
#' @param src_files A character vector of file paths to the source rasters
#' @param vrt_opts A list of options to pass to the VRT
#' @param pix_fun A character string of the pixel function code
#' @return A character string of the path to the modified VRT
#' @export
#' @details
#' This function will add a pixel function to every band in a VRT file. Then,
#' when passed to gdal warp or translate the pixel function will be applied.
vrt_add_pixfun <- function(
  src,
  vrt_opts = NULL,
  pix_fun = vrtility::numba_median
) {
  v_assert_type(src, "src", c("character", "stac_vrt"), multiple = TRUE)
  v_assert_type(pix_fun, "fun", "function")

  if (rlang::inherits_all(src, "stac_vrt")) {
    src_files <- save_vrt(src)
  }
  band_names <- xml2::read_xml(src_files[1]) |>
    xml2::xml_find_all("//VRTRasterBand") |>
    xml2::xml_attr("Description")

  tvrt <- xml2::read_xml(src_files)

  # Modify the first band to add pixel function
  bands <- xml2::xml_find_all(tvrt, "//VRTRasterBand")

  # iterate the bands and add the pixel function
  purrr::walk2(bands, band_names, function(x, y) {
    xml2::xml_set_attr(x, "subClass", "VRTDerivedRasterBand")
    xml2::xml_set_attr(x, "Description", y)

    # Add pixel function elements
    xml2::xml_add_child(x, "PixelFunctionType", "median")
    xml2::xml_add_child(x, "PixelFunctionLanguage", "Python")

    # Add pixel function code
    pixel_func_code <- xml2::xml_add_child(x, "PixelFunctionCode")
    # Add pixel function code as CDATA
    cdata_node <- xml2::xml_cdata(pix_fun())
    xml2::xml_add_child(pixel_func_code, cdata_node)
  })

  return(build_stac_vrt(
    xml = tvrt,
    bbox = src$bbox,
    start_date = src$start_date,
    end_date = src$end_date,
    n_its = src$n_items,
    assets = band_names,
    pixfun = pix_fun()
  ))
}

#' Set the descriptions for the bands in a VRT
#' @param x A character string of the path to the VRT
#' @param descriptions A character vector of the descriptions
#' @param as_file A logical indicating if the VRT should be saved to a file
#' @return A character string of the path to the modified VRT
#' @keywords internal
#' @noRd
set_vrt_descriptions <- function(x, descriptions, as_file = FALSE) {
  mvrt <- xml2::read_xml(x)
  mvrt_bands <- xml2::xml_find_all(mvrt, "//VRTRasterBand")
  purrr::walk2(
    mvrt_bands,
    descriptions,
    function(x, y) {
      xml2::xml_set_attr(x, "Description", y)
    }
  )

  if (as_file) {
    xml2::write_xml(mvrt, x)
    return(x)
  } else {
    return(mvrt)
  }
}

set_vrt_metadata <- function(x, keys, values, as_file = FALSE) {
  v_assert_length(keys, "keys", length(values))
  mvrt <- xml2::read_xml(x)

  metadchild <- xml2::xml_add_child(mvrt, "Metadata")
  purrr::walk2(
    keys,
    values,
    function(x, y) {
      mdi <- xml2::xml_add_child(metadchild, "MDI", y)
      xml2::xml_set_attr(mdi, "key", x)
    }
  )
  if (as_file) {
    xml2::write_xml(mvrt, x)
    return(x)
  } else {
    return(mvrt)
  }
}
