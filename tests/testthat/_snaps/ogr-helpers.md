# ogr_bbox_from_file works

    Code
      bbox_native
    Output
      [1] 469685.73 -12917.76 573531.72  96577.34

---

    Code
      bbox_latlon
    Output
      [1] -111.15605   44.12164 -109.83054   45.11839

---

    Code
      bbox_extended
    Output
      [1] -111.25605   43.92164 -109.73054   45.31839

# ogr_srs_from_file works

    Code
      srs_wkt
    Output
      [1] "PROJCS[\"NAD83 / Montana\",GEOGCS[\"NAD83\",DATUM[\"North_American_Datum_1983\",SPHEROID[\"GRS 1980\",6378137,298.257222101,AUTHORITY[\"EPSG\",\"7019\"]],AUTHORITY[\"EPSG\",\"6269\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4269\"]],PROJECTION[\"Lambert_Conformal_Conic_2SP\"],PARAMETER[\"latitude_of_origin\",44.25],PARAMETER[\"central_meridian\",-109.5],PARAMETER[\"standard_parallel_1\",49],PARAMETER[\"standard_parallel_2\",45],PARAMETER[\"false_easting\",600000],PARAMETER[\"false_northing\",0],UNIT[\"metre\",1,AUTHORITY[\"EPSG\",\"9001\"]],AXIS[\"Easting\",EAST],AXIS[\"Northing\",NORTH],AUTHORITY[\"EPSG\",\"32100\"]]"

