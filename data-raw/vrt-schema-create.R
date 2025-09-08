## code to prepare `vrt_schema` dataset goes here

schema_local <- fs::file_temp(ext = "xsd")
download.file(
  "https://raw.githubusercontent.com/OSGeo/gdal/master/frmts/vrt/data/gdalvrt.xsd",
  schema_local
)

vrt_xml_schema <- paste(readLines(schema_local), collapse = "\n")

# check it reads okay
vrtsch <- xml2::read_xml(vrt_xml_schema)

# find schema definitions for SimpleSourceType and ComplexSourceType

purrr::walk(
  c("ComplexSourceType", "SimpleSourceType"),
  function(typ) {
    sch_bit <- xml2::xml_find_all(
      vrtsch,
      paste0('//xs:complexType[@name="', typ, '"]')
    )

    xml2::xml_add_child(
      sch_bit,
      "xs:attribute",
      name = "name",
      type = "xs:string"
    )
  }
)

xmltemp <- fs::file_temp(ext = "xsd")

xml2::write_xml(vrtsch, xmltemp)


vrt_xml_schema <- paste(readLines(xmltemp), collapse = "\n")

# check modifications propogate.
# x <- xml2::read_xml(vrt_xml_schema)
# y <- xml2::xml_find_all(x, '//xs:complexType[@name="ComplexSourceType"]')
# cat(as.character(y), sep = "\n")

usethis::use_data(vrt_xml_schema, overwrite = TRUE)
