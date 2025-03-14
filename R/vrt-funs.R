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
      desc <- xml2::xml_find_first(x, ".//Description")
      if (is.na(desc)) {
        desc <- xml2::xml_add_child(x, "Description", y)
      }
      xml2::xml_set_text(desc, y)
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
  no_data_node <- xml2::xml_find_first(x, ".//NoDataValue")
  if (!is.na(xml2::xml_text(no_data_node))) {
    xml2::xml_remove(no_data_node)
  }
}
