# creating a test dataset based around Exeter, UK
devtools::load_all()
s2ddir <- fs::dir_create("inst/s2-data")

bbox <- gdalraster::bbox_from_wkt(
  wkt = wk::wkt("POINT (-3.51 50.72)"),
  extend_x = 0.05,
  extend_y = 0.03
)

te <- bbox_to_projected(bbox)
trs <- attr(te, "wkt")

s2_stac <- sentinel2_stac_query(
  bbox = bbox,
  start_date = "2024-06-01",
  end_date = "2024-08-30",
  max_cloud_cover = 50,
  assets = c("B02", "B03", "B04", "SCL")
)
# number of items:
length(s2_stac$features)

ex_collect <- vrt_collect(s2_stac)
ex_collect_warp <- vrt_warp(
  ex_collect,
  t_srs = trs,
  te = te,
  tr = c(20, 20)
)

exe_tiflist <- vrt_compute(
  ex_collect_warp,
  outfile = fs::path(s2ddir, "exe.tif"),
  quiet = TRUE
)

# now let's reproject a couple of the rasters to different projections
newp <- paste0(fs::path_ext_remove(exe_tiflist[1]), "_EPSG27700.tif")
gdalraster::warp(exe_tiflist[1], newp, t_srs = "EPSG:27700")

newp2 <- paste0(fs::path_ext_remove(exe_tiflist[2]), "_UTM.tif")
gdalraster::warp(exe_tiflist[2], newp2, t_srs = ex_collect$srs)

# delete the originals.
fs::file_delete(exe_tiflist[1:2])
