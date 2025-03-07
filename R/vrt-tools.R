#' Load the official GDAL VRT XML schema
#' @description Loads a copy of the official xml schema from the provided
#' vrt_xml_schema dataset.
#' @rdname vrt_tools
#' @return An xml_document object
#' @examples
#' vrt_schema()
#' @export
vrt_schema <- function() {
  xml2::read_xml(vrt_xml_schema)
}
