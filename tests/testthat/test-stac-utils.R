test_that("hls_stac_query works", {
  skip_if_offline()
  skip_on_cran()
  bbox <- gdalraster::bbox_from_wkt(
    wkt = "POINT (144.3 -7.6)",
    extend_x = 0.17,
    extend_y = 0.125
  )
  te <- bbox_to_projected(bbox)
  trs <- attr(te, "wkt")

  hls_query <- hls_stac_query(
    bbox,
    start_date = "2023-01-01",
    end_date = "2023-02-28",
    assets = c("B04", "B03", "B02", "Fmask"),
    collection = "HLSS30_2.0",
    max_cloud_cover = 70
  )
  expect_s3_class(hls_query, "doc_items")
  expect_equal(length(hls_query$features), 5)
})
