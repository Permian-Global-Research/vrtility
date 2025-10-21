# Helper functions for testing ===============================================

# Create mock STAC items for testing
create_mock_stac_items <- function(n = 3, collection = "test-collection") {
  features <- list()
  for (i in seq_len(n)) {
    features[[i]] <- list(
      type = "Feature",
      id = paste0("item-", i),
      collection = collection,
      bbox = c(-1 + i * 0.1, 50 + i * 0.1, 1 + i * 0.1, 52 + i * 0.1),
      properties = list(
        datetime = paste0("2023-01-", sprintf("%02d", i), "T12:00:00Z"),
        `eo:cloud_cover` = i * 10,
        `sat:orbit_state` = if (i %% 2 == 0) "ascending" else "descending",
        platform = "test-platform"
      ),
      assets = list(
        B04 = list(href = paste0("http://example.com/item-", i, "/B04.tif")),
        B03 = list(href = paste0("http://example.com/item-", i, "/B03.tif"))
      )
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

# Unit tests for helper functions ============================================

test_that("format_stac_date formats dates correctly", {
  expect_equal(
    vrtility:::format_stac_date("2023-01-15"),
    "2023-01-15T00:00:00Z"
  )

  expect_equal(
    vrtility:::format_stac_date("2023-01-15T12:30:45"),
    "2023-01-15T12:30:45Z"
  )
})

test_that("execute_stac_request handles GET/POST logic", {
  skip_if_offline()

  # Test with a real but simple request (should use GET)
  stac_endpoint <- rstac::stac(
    "https://planetarycomputer.microsoft.com/api/stac/v1/"
  )
  search <- rstac::stac_search(
    stac_endpoint,
    collections = "sentinel-2-l2a",
    bbox = c(-1, 50, 1, 52)
  )

  # Should execute without error
  expect_no_error({
    result <- vrtility:::execute_stac_request(search)
    expect_s3_class(result, "rstac_doc")
  })
})

# Tests for filtering functions ===============================================

test_that("stac_cloud_filter works correctly", {
  mock_items <- create_mock_stac_items(5)

  # Filter for cloud cover < 25%
  filtered <- stac_cloud_filter(mock_items, 25)

  expect_s3_class(filtered, "doc_items")
  expect_equal(length(filtered$features), 2) # Items 1 and 2 have 10% and 20%

  # Check that correct items remain
  cloud_covers <- purrr::map_dbl(
    filtered$features,
    ~ .x$properties$`eo:cloud_cover`
  )
  expect_true(all(cloud_covers <= 25))
})

test_that("stac_orbit_filter works correctly", {
  mock_items <- create_mock_stac_items(4)

  # Filter for ascending only
  filtered <- stac_orbit_filter(mock_items, "ascending")

  expect_s3_class(filtered, "doc_items")
  expect_equal(length(filtered$features), 2) # Items 2 and 4 are ascending

  # Check that correct items remain
  orbit_states <- purrr::map_chr(
    filtered$features,
    ~ .x$properties$`sat:orbit_state`
  )
  expect_true(all(orbit_states == "ascending"))

  # Test multiple orbit states
  filtered_both <- stac_orbit_filter(mock_items, c("ascending", "descending"))
  expect_equal(length(filtered_both$features), 4) # All items
})

test_that("stac_coverage_filter works correctly", {
  skip_if_not(
    gdalraster::gdal_version_num() >= gdalraster::gdal_compute_version(3, 9, 0),
    message = "GDAL >= 3.9.0 required for stac_coverage_filter tests"
  )
  # Create items with known bounding boxes
  mock_items <- create_mock_stac_items(3)

  # Test bbox that should intersect well with item 1
  test_bbox <- c(-0.9, 50.1, 1.1, 52.1)

  filtered <- stac_coverage_filter(mock_items, test_bbox, min_coverage = 0.1)

  expect_s3_class(filtered, "doc_items")
  expect_gte(length(filtered$features), 1) # At least one item should pass
})

test_that("stac_drop_duplicates removes duplicates correctly", {
  # Create items where some are duplicates
  mock_items <- create_mock_stac_items(2)

  # Add a duplicate of the first item
  duplicate_item <- mock_items$features[[1]]
  duplicate_item$id <- "item-duplicate"
  mock_items$features[[3]] <- duplicate_item

  filtered <- stac_drop_duplicates(mock_items)

  expect_s3_class(filtered, "doc_items")
  expect_equal(length(filtered$features), 2) # Should remove 1 duplicate
})

# Integration tests with minimal API calls ===================================

test_that("stac_query basic functionality works", {
  skip_if_offline()

  # Single minimal request to test basic functionality
  bbox <- c(144.130, -7.725, 144.470, -7.475)

  result <- stac_query(
    bbox = bbox,
    stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
    collection = "sentinel-2-l2a",
    start_date = "2023-01-01",
    end_date = "2023-01-15", # Very short date range
    check_collection = FALSE # Skip collection check to save a request
  )

  expect_s3_class(result, "doc_items")
  expect_true(length(result$features) <= 6)

  # Test that all items are from correct collection
  collections <- purrr::map_chr(result$features, ~ .x$collection)
  expect_true(all(collections == "sentinel-2-l2a"))
})


test_that("sentinel2_stac_query works", {
  skip_if_offline()

  bbox <- c(-1, 50, 1, 52)

  result <- sentinel2_stac_query(
    bbox = bbox,
    start_date = "2023-06-01",
    end_date = "2023-06-15",
    assets = c("B04", "B03", "B02"),
    max_cloud_cover = 50
  )

  expect_s3_class(result, "doc_items")

  # Test that cloud filtering was applied
  if (length(result$features) > 0) {
    cloud_covers <- purrr::map_dbl(
      result$features,
      ~ .x$properties$`eo:cloud_cover`
    )
    expect_true(all(cloud_covers <= 50))
  }
})

test_that("hls_stac_query works", {
  skip_if_offline()

  bbox <- c(144.3, -7.6, 144.4, -7.5) # Small bbox in Indonesia

  expect_error(hls_stac_query(
    bbox = bbox,
    start_date = "2023-01-01",
    end_date = "2023-01-05", # Very short range
    assets = c("B04", "B03", "B02"),
    collection = "HLSS30_2.0",
    max_cloud_cover = 70
  ))

  result <- hls_stac_query(
    bbox = bbox,
    start_date = "2023-01-01",
    end_date = "2023-03-15", # Very short range
    assets = c("B04", "B03", "B02"),
    collection = "hls2-s30",
    max_cloud_cover = 70
  )

  expect_s3_class(result, "doc_items")
})

test_that("sentinel1_stac_query works", {
  skip_if_offline()

  bbox <- c(-1, 50, 1, 52)

  result <- sentinel1_stac_query(
    bbox = bbox,
    start_date = "2023-06-01",
    end_date = "2023-07-02",
    assets = "vv",
    orbit_state = "descending"
  )

  expect_s3_class(result, "doc_items")

  # Test that orbit filtering was applied
  if (length(result$features) > 0) {
    orbit_states <- purrr::map_chr(
      result$features,
      ~ .x$properties$`sat:orbit_state`
    )
    expect_true(all(orbit_states == "descending"))
  }
})

# Error handling tests =====================================================

test_that("stac_query validates inputs correctly", {
  expect_error(
    stac_query(
      bbox = c(-1, 50), # Invalid bbox length
      stac_source = "https://example.com",
      collection = "test",
      start_date = "2023-01-01",
      end_date = "2023-01-02"
    ),
    "bbox.*length.*4"
  )

  expect_error(
    stac_query(
      bbox = c(-1, 50, 1, 52),
      stac_source = 123, # Invalid type
      collection = "test",
      start_date = "2023-01-01",
      end_date = "2023-01-02"
    ),
    "stac_source.*character"
  )
})

test_that("filter functions validate inputs correctly", {
  mock_items <- create_mock_stac_items(2)

  # Test stac_cloud_filter validation
  expect_error(
    stac_cloud_filter("not_items", 10),
    "items.*doc_items"
  )

  expect_error(
    stac_cloud_filter(mock_items, "not_numeric"),
    "max_cloud_cover.*numeric"
  )

  # Test stac_coverage_filter validation
  expect_error(
    stac_coverage_filter(mock_items, c(-1, 50), 0.5), # Invalid bbox length
    "bbox.*length.*4"
  )

  expect_error(
    stac_coverage_filter(mock_items, c(-1, 50, 1, 52), 1.5), # Invalid coverage
    "min_coverage.*range"
  )
})

# MPC signing tests (without actual tokens) ===============================

test_that("sign_mpc_items handles empty items gracefully", {
  empty_items <- structure(
    list(type = "FeatureCollection", features = list()),
    class = c("doc_items", "rstac_doc", "list")
  )

  expect_warning(
    result <- sign_mpc_items(empty_items)
  )

  expect_s3_class(result, "doc_items")
  expect_equal(length(result$features), 0)
})

# Asset filtering tests ===================================================

test_that("asset filtering works in stac_query", {
  skip_if_offline()

  bbox <- c(-1, 50, 1, 52)

  # Test with specific assets
  result <- stac_query(
    bbox = bbox,
    stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
    collection = "sentinel-2-l2a",
    start_date = "2023-06-01",
    end_date = "2023-06-05",
    assets = c("B04", "B03"),
    check_collection = FALSE
  )

  expect_s3_class(result, "doc_items")

  # Check that only requested assets are present
  asset_names <- names(result$features[[1]]$assets)
  expect_true(all(asset_names %in% c("B04", "B03")))
})

# Edge cases and robustness tests ==========================================

test_that("functions handle edge cases gracefully", {
  # Test with empty results
  empty_items <- structure(
    list(type = "FeatureCollection", features = list()),
    class = c("doc_items", "rstac_doc", "list")
  )

  expect_no_error({
    result1 <- stac_cloud_filter(empty_items, 10)
    result2 <- stac_orbit_filter(empty_items, "ascending")
  })

  expect_warning(
    {
      result3 <- stac_drop_duplicates(empty_items)
    },
    "There are no items from which to drop duplicates"
  )

  expect_s3_class(result1, "doc_items")
  expect_s3_class(result2, "doc_items")
  expect_s3_class(result3, "doc_items")
  expect_equal(length(result1$features), 0)
  expect_equal(length(result2$features), 0)
  expect_equal(length(result3$features), 0)
})

test_that("date formatting handles various inputs", {
  expect_no_error(vrtility:::format_stac_date("2023-01-01"))
  expect_no_error(vrtility:::format_stac_date("2023-01-01T12:00:00"))
  expect_no_error(vrtility:::format_stac_date("2023-01-01T12:00:00Z"))

  # Test with lubridate-parseable formats
  expect_no_error(vrtility:::format_stac_date("2023/01/01"))
})
