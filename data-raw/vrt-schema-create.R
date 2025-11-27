## code to prepare `vrt_schema` dataset goes here

schema_local <- fs::file_temp(ext = "xsd")
# nolint: download_file
download.file(
  "https://raw.githubusercontent.com/OSGeo/gdal/master/frmts/vrt/data/gdalvrt.xsd",
  schema_local
)

vrt_xml_schema <- paste(readLines(schema_local), collapse = "\n")

# check it reads okay
vrtsch <- xml2::read_xml(vrt_xml_schema)

usethis::use_data(vrt_xml_schema, overwrite = TRUE)
