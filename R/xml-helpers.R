drop_nodatavalue <- function(x) {
  no_data_node <- xml2::xml_find_first(x, ".//NoDataValue")
  if (!is.na(xml2::xml_text(no_data_node))) {
    xml2::xml_remove(no_data_node)
  }
}

drop_nodata <- function(x) {
  no_data_node <- xml2::xml_find_first(x, ".//NODATA")
  if (!is.na(xml2::xml_text(no_data_node))) {
    xml2::xml_remove(no_data_node)
  }
}

set_use_maskband <- function(x) {
  srcs <- xml2::xml_find_all(
    x,
    ".//SimpleSource | .//ComplexSource"
  )

  purrr::map(srcs, function(x) {
    umb <- xml2::xml_add_child(x, "UseMaskBand")
    xml2::xml_set_text(umb, "true")
  })
}
