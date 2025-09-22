#' Load the official GDAL VRT XML schema
#' @param schema a character string of the xml schema - mainly intended only
#' for use with the official gdal VRT schema.
#' @description Loads a copy of the official xml schema from the provided
#' vrt_xml_schema dataset.
#' @rdname vrt_tools
#' @return An xml_document object
#' @examples
#' vrt_schema()
#' @export
vrt_schema <- function(schema = vrtility::vrt_xml_schema) {
  xml2::read_xml(schema)
}


#' Iternal function to set the descriptions for the bands in a VRT
#' @param x A character string of the path to the VRT
#' @param descriptions A character vector of the descriptions
#' @param as_file A logical indicating if the VRT should be saved to a file
#' @return A character string of the path to the modified VRT
#' @keywords internal
set_vrt_descriptions <- function(x, descriptions, as_file = FALSE) {
  mvrt <- xml2::read_xml(x)
  mvrt_bands <- xml2::xml_find_all(mvrt, "//VRTRasterBand")
  purrr::walk2(
    mvrt_bands,
    descriptions,
    function(x, y) {
      desc <- xml2::xml_find_first(x, ".//Description")
      if (is.na(desc)) {
        desc <- xml2::xml_add_child(x, "Description", y)
      }
      xml2::xml_set_text(desc, y)

      src <- vrt_find_all_srcs(x)
      xml2::xml_set_attr(src, "name", y)
    }
  )

  if (as_file) {
    xml2::write_xml(mvrt, x)
    return(x)
  } else {
    return(mvrt)
  }
}

#' Internal function to set metadata in a VRT
#' @param x A character string of the path to the VRT
#' @param keys A character vector of the metadata keys
#' @param values A character vector of the metadata values
#' @param as_file A logical indicating if the VRT should be saved to a file
#' @return An xml_document object
#' @keywords internal
#' @export
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

#' @keywords internal
#' @noRd
drop_nodatavalue <- function(x) {
  purrr::walk(c(".//NoDataValue", ".//NODATA"), function(ndv) {
    no_data_node <- xml2::xml_find_all(x, ndv)
    xml2::xml_remove(no_data_node)
  })
}

#' @keywords internal
#' @noRd
drop_scale <- function(x) {
  purrr::walk(c(".//Scale", ".//Offset"), function(v) {
    scale_node <- xml2::xml_find_all(x, v)
    xml2::xml_remove(scale_node)
  })
}

#' @keywords internal
#' @noRd
set_nodatavalue <- function(
  x,
  value,
  nodata_targets = c(".//NoDataValue", ".//NODATA")
) {
  purrr::walk(nodata_targets, function(ndv) {
    no_data_node <- xml2::xml_find_all(x, ndv)
    xml2::xml_set_text(no_data_node, as.character(value))
  })
}


#' @noRd
#' @keywords internal
#' @description Check if a pixel function is already set at the VRT level
#' @param x A VRT object - could be a whole vrt xml or a RasterBand.
check_for_pixel_fun <- function(x) {
  existing_pf <- xml2::xml_find_all(x, ".//PixelFunctionType")
  if (length(existing_pf) > 0) {
    cli::cli_abort(
      c(
        "x" = "A Pixel function is already set At this vrt-level.",
        "i" = "You must save/warp this file before setting another one."
      )
    )
  }
  invisible()
}

#' Reset an XML element in a parent element
#' @param element_parent The parent XML element to which the new element will
#' be added.
#' @param element_name The name of the element to be reset.
#' @param value The value to set for the new element.
#' @noRd
#' @keywords internal
reset_element <- function(element_parent, element_name, value) {
  xml2::xml_remove(xml2::xml_find_all(
    element_parent,
    glue::glue(".//{element_name}")
  ))
  xml2::xml_add_child(
    element_parent,
    element_name,
    format(value, scientific = FALSE)
  )
}


vrt_find_all_srcs <- function(x) {
  xml2::xml_find_all(
    x,
    ".//SimpleSource | .//ComplexSource"
  )
}

vrt_find_first_src <- function(x) {
  xml2::xml_find_first(
    x,
    ".//SimpleSource | .//ComplexSource"
  )
}


vrt_collapse <- function(vrt) {
  vrt_xml <- xml2::read_xml(vrt)

  vrt_bands <- xml2::xml_find_all(vrt_xml, ".//VRTRasterBand")
  vrt_srcs <- vrt_find_all_srcs(vrt_xml)

  xml2::xml_remove(vrt_bands[seq_along(vrt_bands)[-1]])

  vrt_b1_srcs <- vrt_find_all_srcs(vrt_xml)
  xml2::xml_remove(vrt_b1_srcs)

  vrt_b1 <- xml2::xml_find_first(vrt_xml, ".//VRTRasterBand")
  purrr::walk(vrt_srcs, ~ xml2::xml_add_child(vrt_b1, .x))

  xml2::write_xml(vrt_xml, vrt)
  return(invisible(vrt))
}


vrt_squish_bands <- function(band_files, inbands, vrt) {
  if (length(band_files) == 1) {
    gdalraster::buildVRT(
      vrt,
      band_files,
      cl_arg = c(unlist(purrr::map(inbands, ~ c("-b", .x)))),
      quiet = TRUE
    )

    vrt_collapse(vrt)
  } else {
    msk_file <- band_files[inbands]
    gdalraster::buildVRT(vrt, msk_file, quiet = TRUE)
  }

  return(invisible(vrt))
}
