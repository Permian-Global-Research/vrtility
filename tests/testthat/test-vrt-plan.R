# Helper function to create mock STAC items for vrt_plan testing
create_mock_stac_items_for_plan <- function(n = 3) {
  features <- list()
  for (i in seq_len(n)) {
    xmin <- -1 + i * 0.1
    ymin <- 50 + i * 0.1
    xmax <- 1 + i * 0.1
    ymax <- 52 + i * 0.1

    features[[i]] <- structure(
      list(
        type = "Feature",
        stac_version = "1.0.0",
        id = paste0("item-", i),
        collection = "test-collection",
        bbox = c(xmin, ymin, xmax, ymax),
        geometry = list(
          type = "Polygon",
          coordinates = list(list(
            c(xmin, ymin),
            c(xmax, ymin),
            c(xmax, ymax),
            c(xmin, ymax),
            c(xmin, ymin)
          ))
        ),
        properties = list(
          datetime = paste0("2023-01-", sprintf("%02d", i), "T12:00:00Z"),
          `eo:cloud_cover` = i * 10,
          platform = "test-platform"
        ),
        assets = list(
          B02 = list(href = paste0("http://example.com/item-", i, "/B02.tif")),
          B03 = list(href = paste0("http://example.com/item-", i, "/B03.tif")),
          B04 = list(href = paste0("http://example.com/item-", i, "/B04.tif"))
        ),
        links = list()
      ),
      class = c("doc_item", "rstac_doc", "list")
    )
  }

  structure(
    list(
      type = "FeatureCollection",
      features = features
    ),
    class = c("doc_items", "rstac_doc", "list")
  )
}

# Test vrt_plan.default error handling ========================================

test_that("vrt_plan.default errors for unsupported classes", {
  expect_error(
    vrt_plan("not_a_stac_object"),
    "character.*not supported.*vrt_plan"
  )

  expect_error(
    vrt_plan(list(a = 1, b = 2)),
    "list.*not supported.*vrt_plan"
  )

  expect_error(
    vrt_plan(data.frame(x = 1:3)),
    "data.frame.*not supported.*vrt_plan"
  )
})

# Test vrt_plan.doc_items =====================================================

test_that("vrt_plan creates valid vrt_plan object from mock STAC items", {
  mock_items <- create_mock_stac_items_for_plan(3)

  plan <- vrt_plan(mock_items)

  # Check class

  expect_s3_class(plan, "vrt_plan")
  expect_s3_class(plan, "list")

  # Check structure
  expect_named(
    plan,
    c("sources", "assets", "date_time", "n_items", "vsi_prefix", "driver")
  )

  # Check values
  expect_equal(plan$n_items, 3)
  expect_equal(length(plan$sources), 3)
  expect_equal(length(plan$date_time), 3)
  expect_true(all(c("B02", "B03", "B04") %in% plan$assets))
})

test_that("vrt_plan extracts correct assets", {
  mock_items <- create_mock_stac_items_for_plan(2)

  plan <- vrt_plan(mock_items)

  # Assets should be extracted and sorted
  expect_equal(sort(plan$assets), c("B02", "B03", "B04"))
})

test_that("vrt_plan extracts and sorts by datetime", {
  mock_items <- create_mock_stac_items_for_plan(3)

  plan <- vrt_plan(mock_items)

  # Datetimes should be sorted chronologically
  parsed_dates <- lubridate::as_datetime(plan$date_time)
  expect_true(all(diff(parsed_dates) >= 0))
})

test_that("vrt_plan stores source URIs correctly", {
  mock_items <- create_mock_stac_items_for_plan(2)

  plan <- vrt_plan(mock_items)

  # Check structure of sources
  expect_equal(length(plan$sources), 2)

  # Each item should have URIs for all assets
  first_item <- plan$sources[[1]]
  expect_true(all(plan$assets %in% names(first_item)))

  # Check URI format
  first_uri <- first_item[["B02"]]$uri
  expect_true(grepl("B02\\.tif$", first_uri))
})

test_that("vrt_plan respects vsi_prefix argument", {
  mock_items <- create_mock_stac_items_for_plan(1)

  plan <- vrt_plan(mock_items, vsi_prefix = "/vsicurl/")

  expect_equal(plan$vsi_prefix, "/vsicurl/")

  # URIs should have the prefix
  first_uri <- plan$sources[[1]][["B02"]]$uri
  expect_true(grepl("^/vsicurl/", first_uri))
})

test_that("vrt_plan stores driver argument", {
  mock_items <- create_mock_stac_items_for_plan(1)

  plan <- vrt_plan(mock_items, driver = "")

  expect_equal(plan$driver, "")
})

# Test build_vrt_plan internal constructor ====================================

test_that("build_vrt_plan creates correct structure", {
  sources <- list(
    list(B02 = list(uri = "test1.tif", dttm = "2023-01-01")),
    list(B02 = list(uri = "test2.tif", dttm = "2023-01-02"))
  )

  plan <- vrtility:::build_vrt_plan(
    sources = sources,
    assets = "B02",
    date_time = c("2023-01-01", "2023-01-02"),
    vsi_prefix = "",
    driver = ""
  )

  expect_s3_class(plan, "vrt_plan")
  expect_equal(plan$n_items, 2)
  expect_equal(plan$assets, "B02")
})

# Test print.vrt_plan =========================================================

test_that("print.vrt_plan runs without error", {
  mock_items <- create_mock_stac_items_for_plan(3)
  plan <- vrt_plan(mock_items)

  expect_no_error(capture.output(print(plan)))
})

test_that("print.vrt_plan returns invisible", {
  mock_items <- create_mock_stac_items_for_plan(2)
  plan <- vrt_plan(mock_items)

  result <- capture.output(printed <- print(plan))

  expect_identical(printed, plan)
})

# Test sources_printer helper =================================================

test_that("sources_printer truncates long URLs", {
  long_url <- paste0(rep("a", 100), collapse = "")
  sources <- list(
    list(B02 = list(uri = long_url, dttm = "2023-01-01"))
  )

  output <- vrtility:::sources_printer(sources)

  # Should be truncated with ...
  expect_true(grepl("\\.\\.\\.$", output))
  expect_true(nchar(output) < nchar(long_url) + 50)
})

test_that("sources_printer handles short URLs", {
  short_url <- "http://example.com/test.tif"
  sources <- list(
    list(B02 = list(uri = short_url, dttm = "2023-01-01"))
  )

  output <- vrtility:::sources_printer(sources)

  # Should contain the full URL
  expect_true(grepl(short_url, output, fixed = TRUE))
})

# Test edge cases =============================================================

test_that("vrt_plan handles single item", {
  mock_items <- create_mock_stac_items_for_plan(1)

  plan <- vrt_plan(mock_items)

  expect_equal(plan$n_items, 1)
  expect_equal(length(plan$sources), 1)
  expect_equal(length(plan$date_time), 1)
})

test_that("vrt_plan handles items with duplicate datetimes", {
  mock_items <- create_mock_stac_items_for_plan(2)
  # Set same datetime for both items
  mock_items$features[[2]]$properties$datetime <- mock_items$features[[
    1
  ]]$properties$datetime

  expect_no_error({
    plan <- vrt_plan(mock_items)
  })

  expect_equal(plan$n_items, 2)
})

# Integration test with vrt_warp (if applicable) ==============================

test_that("vrt_plan integrates with vrt_warp", {
  skip_if_offline()
  skip_on_cran()

  # Use a real but minimal STAC query
  bbox <- c(144.3, -7.6, 144.35, -7.55)

  stac_items <- tryCatch(
    {
      hls_stac_query(
        bbox = bbox,
        start_date = "2023-06-01",
        end_date = "2023-06-30",
        stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
        collection = "hls2-s30",
        max_cloud_cover = 100,
        assets = c("B02", "B03", "B04", "Fmask")
      )
    },
    error = function(e) NULL
  )

  skip_if(is.null(stac_items), "Could not fetch STAC items")
  skip_if(length(stac_items$features) == 0, "No STAC items found")

  # Create plan
  plan <- vrt_plan(stac_items)

  expect_s3_class(plan, "vrt_plan")
  expect_true(plan$n_items > 0)

  # Test that vrt_warp can accept vrt_plan
  te <- bbox_to_projected(bbox)
  trs <- attr(te, "wkt")

  warped <- vrt_warp(
    plan,
    t_srs = trs,
    te = te,
    tr = c(30, 30)
  )

  expect_s3_class(warped, "vrt_collection_warped")
  expect_equal(warped$n_items, plan$n_items)
})
