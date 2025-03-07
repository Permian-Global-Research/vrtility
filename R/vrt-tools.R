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
