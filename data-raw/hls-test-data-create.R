# creating a test dataset based around Exeter, UK
devtools::load_all()
mirai::daemons(10)
s2ddir <- fs::dir_create("inst/hls-data")
fs::dir_ls(s2ddir, recurse = TRUE) |>
  fs::file_delete()

bbox <- gdalraster::bbox_from_wkt(
  wkt = "POINT (-3.51 50.72)",
  extend_x = 0.05,
  extend_y = 0.03
)

te <- bbox_to_projected(bbox)
trs <- attr(te, "wkt")

hls_stac <- hls_stac_query(
  bbox = bbox,
  start_date = "2021-06-01",
  end_date = "2021-08-30",
  max_cloud_cover = 50,
  assets = c("B02", "B03", "B04", "B08", "Fmask"),
  collection = "hls2-s30"
)

hls_collect <- vrt_collect(hls_stac)
hls_collect_warp <- vrt_warp(
  hls_collect,
  t_srs = trs,
  te = te,
  tr = c(30, 30),
  lazy = TRUE
)

plot(hls_collect_warp, item = 1, bands = c(4, 2, 1))
plot(hls_collect_warp, item = 3, bands = 5)

vrt_compute(
  hls_collect_warp,
  outfile = fs::path(s2ddir, "hls_exe.tif"),
  quiet = TRUE,
  dst_nodata = -9999
)


plot_raster_src(
  "inst/hls-data/hls_exe_2021-06-08_12-27-08.tif",
  bands = 5
)
