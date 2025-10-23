# Test numeric vector input
test_that("r_to_MEM works with numeric vectors", {
  test_vec <- sin(seq(0, 2 * pi, length.out = 100))

  # Test with all required parameters
  mem_ds <- r_to_MEM(
    test_vec,
    nbands = 1,
    xsize = 10,
    ysize = 10
  )

  expect_s4_class(mem_ds, "Rcpp_GDALRaster")
  expect_equal(mem_ds$getRasterXSize(), 10)
  expect_equal(mem_ds$getRasterYSize(), 10)
  expect_equal(mem_ds$getRasterCount(), 1)
  expect_equal(mem_ds$getDataTypeName(1), "Float32")

  # Clean up
  mem_ds$close()
})

test_that("r_to_MEM works with matrices", {
  test_mat <- matrix(runif(100), nrow = 10, ncol = 10)

  mem_ds <- r_to_MEM(test_mat)

  expect_s4_class(mem_ds, "Rcpp_GDALRaster")
  expect_equal(mem_ds$getRasterXSize(), 10) # nrow becomes xsize
  expect_equal(mem_ds$getRasterYSize(), 10) # ncol becomes ysize
  expect_equal(mem_ds$getRasterCount(), 1)

  # Test data integrity
  read_data <- gdalraster::read_ds(
    mem_ds,
    bands = 1,
    xoff = 0,
    yoff = 0,
    xsize = 10,
    ysize = 10
  )
  expect_equal(length(read_data), 100)

  mem_ds$close()
})

test_that("r_to_MEM works with arrays", {
  test_array <- array(runif(120), dim = c(10, 4, 3))

  mem_ds <- r_to_MEM(test_array)

  expect_s4_class(mem_ds, "Rcpp_GDALRaster")
  expect_equal(mem_ds$getRasterXSize(), 10) # first dimension
  expect_equal(mem_ds$getRasterYSize(), 4) # second dimension
  expect_equal(mem_ds$getRasterCount(), 3) # third dimension

  # Test each band
  for (band in 1:3) {
    read_data <- gdalraster::read_ds(
      mem_ds,
      bands = band,
      xoff = 0,
      yoff = 0,
      xsize = 10,
      ysize = 4
    )
    expect_equal(length(read_data), 40)
  }

  mem_ds$close()
})

test_that("r_to_MEM handles spatial parameters correctly", {
  test_mat <- matrix(1:25, nrow = 5, ncol = 5)
  bbox <- c(0, 0, 100, 100) # xmin, ymin, xmax, ymax
  srs <- "EPSG:4326"

  mem_ds <- r_to_MEM(
    test_mat,
    bbox = bbox,
    srs = srs
  )

  expect_s4_class(mem_ds, "Rcpp_GDALRaster")

  # Check geotransform
  gt <- mem_ds$getGeoTransform()
  expect_equal(gt[1], 0) # top left x
  expect_equal(gt[2], 20) # pixel width (100/5)
  expect_equal(gt[4], 100) # top left y
  expect_equal(gt[6], -20) # pixel height (negative)

  # Check projection - allow for different WKT formats by checking it's not empty
  proj_string <- mem_ds$getProjection()
  expect_true(nzchar(proj_string))
  expect_true(grepl("WGS.*84|EPSG.*4326", proj_string))

  mem_ds$close()
})

test_that("r_to_MEM handles data types correctly", {
  test_vec <- 1:100

  # Test with explicit data type
  mem_ds <- r_to_MEM(
    test_vec,
    nbands = 1,
    xsize = 10,
    ysize = 10,
    data_type = "Int32"
  )

  expect_equal(mem_ds$getDataTypeName(1), "Int32")

  mem_ds$close()

  # Test default data type (should be Float32)
  mem_ds2 <- r_to_MEM(
    test_vec,
    nbands = 1,
    xsize = 10,
    ysize = 10
  )

  expect_equal(mem_ds2$getDataTypeName(1), "Float32")

  mem_ds2$close()
})

test_that("r_to_MEM handles NoData values correctly", {
  test_data <- c(1, 2, NA, 4, 5, NA, 7, 8, 9)

  # Test with explicit NoData value
  mem_ds <- r_to_MEM(
    test_data,
    nbands = 1,
    xsize = 3,
    ysize = 3,
    no_data_value = -9999
  )

  expect_equal(mem_ds$getNoDataValue(1), -9999)

  # Read data and check NA replacement
  read_data <- gdalraster::read_ds(
    mem_ds,
    bands = 1,
    xoff = 0,
    yoff = 0,
    xsize = 3,
    ysize = 3
  )
  # For MEM datasets with explicit NoData, the values should be preserved
  # The positions 3 and 6 correspond to where we put NA values
  expect_true(all(is.na(read_data[c(3, 6)]) | read_data[c(3, 6)] == -9999))

  mem_ds$close()

  # Test default NoData handling
  mem_ds2 <- r_to_MEM(
    test_data,
    nbands = 1,
    xsize = 3,
    ysize = 3
  )

  # For Float32, default NoData should be NA (handled by GDAL)
  nodata_val <- mem_ds2$getNoDataValue(1)
  expect_true(is.na(nodata_val))

  mem_ds2$close()
})

test_that("r_to_MEM works with gdalraster derived data", {
  s2_img <- fs::dir_ls(system.file("s2-data", package = "vrtility"))[1]
  ds <- methods::new(gdalraster::GDALRaster, s2_img)

  # Read a small subset to test
  data <- gdalraster::read_ds(
    ds,
    xoff = 0,
    yoff = 0,
    xsize = 50,
    ysize = 50,
    out_xsize = 50,
    out_ysize = 50
  )

  ds$close()

  # Data should have 'gis' attribute with spatial metadata
  expect_true(!is.null(attr(data, "gis")))

  # Create MEM dataset - spatial parameters should be extracted automatically
  mem_ds <- r_to_MEM(data, no_data_value = 0)

  expect_s4_class(mem_ds, "Rcpp_GDALRaster")
  expect_equal(mem_ds$getRasterXSize(), 50)
  expect_equal(mem_ds$getRasterYSize(), 50)
  expect_equal(mem_ds$getNoDataValue(1), 0)

  # Check that spatial reference is preserved (allow for different WKT formats)
  gis_attr <- attr(data, "gis")
  if (!is.null(gis_attr$srs) && nzchar(gis_attr$srs)) {
    proj_string <- mem_ds$getProjection()
    expect_true(nzchar(proj_string))
    # Basic check that it contains some projection info
    expect_true(grepl("PROJ|GEOG", proj_string))
  }

  mem_ds$close()
})

test_that("r_to_MEM error handling works correctly", {
  test_vec <- 1:100

  # Test missing required parameters for numeric vector
  expect_error(
    r_to_MEM(test_vec),
    class = "missing_dimensions_error"
  )

  expect_error(
    r_to_MEM(test_vec, nbands = 1),
    class = "missing_dimensions_error"
  )

  expect_error(
    r_to_MEM(test_vec, nbands = 1, xsize = 10),
    class = "missing_dimensions_error"
  )

  # Test invalid data type
  expect_error(
    r_to_MEM(
      "invalid_data",
      nbands = 1,
      xsize = 10,
      ysize = 10
    ),
    class = "invalid_r_to_MEM_data_type_error"
  )

  # Test invalid bbox
  expect_error(
    r_to_MEM(
      matrix(1:25, 5, 5),
      bbox = c(1, 2, 3) # wrong length
    ),
    class = "vrtility_length_error"
  )
})

test_that("r_to_MEM handles edge cases", {
  # Test single pixel
  single_pixel <- matrix(42, nrow = 1, ncol = 1)
  mem_ds <- r_to_MEM(single_pixel)

  expect_equal(mem_ds$getRasterXSize(), 1)
  expect_equal(mem_ds$getRasterYSize(), 1)

  single_read <- gdalraster::read_ds(
    mem_ds,
    bands = 1,
    xoff = 0,
    yoff = 0,
    xsize = 1,
    ysize = 1
  )
  expect_equal(as.numeric(single_read), 42)

  mem_ds$close()

  # Test large array with multiple bands
  large_array <- array(runif(1000), dim = c(10, 10, 10))
  mem_ds_large <- r_to_MEM(large_array)

  expect_equal(mem_ds_large$getRasterCount(), 10)
  expect_equal(mem_ds_large$getRasterXSize(), 10)
  expect_equal(mem_ds_large$getRasterYSize(), 10)

  mem_ds_large$close()

  # Test with all NA data
  na_data <- matrix(rep(NA_real_, 25), nrow = 5, ncol = 5)
  mem_ds_na <- r_to_MEM(na_data, no_data_value = -999)

  read_na <- gdalraster::read_ds(
    mem_ds_na,
    bands = 1,
    xoff = 0,
    yoff = 0,
    xsize = 5,
    ysize = 5
  )
  # NoData values may be returned as NA or the actual NoData value
  expect_true(all(is.na(read_na)))

  expect_true(mem_ds_na$getNoDataValue(1) == -999)

  mem_ds_na$close()
})

test_that("r_to_MEM data integrity is preserved", {
  # Create known test data
  test_matrix <- matrix(1:20, nrow = 4, ncol = 5)

  mem_ds <- r_to_MEM(test_matrix)

  # Read back the data
  read_data <- gdalraster::read_ds(
    mem_ds,
    bands = 1,
    xoff = 0,
    yoff = 0,
    xsize = 4,
    ysize = 5
  )

  # Convert back to matrix for comparison
  read_matrix <- matrix(read_data, nrow = 4, ncol = 5)

  # Data should match (allowing for floating point precision)
  expect_equal(read_matrix, test_matrix, tolerance = 1e-6)

  mem_ds$close()
})

test_that("r_to_MEM geotransform calculations are correct", {
  test_mat <- matrix(1:16, nrow = 4, ncol = 4)
  bbox <- c(100, 200, 300, 400) # xmin=100, ymin=200, xmax=300, ymax=400

  mem_ds <- r_to_MEM(test_mat, bbox = bbox)

  gt <- mem_ds$getGeoTransform()

  expect_equal(gt[1], 100) # top left x (xmin)
  expect_equal(gt[2], 50) # pixel width
  expect_equal(gt[3], 0) # rotation x
  expect_equal(gt[4], 400) # top left y (ymax)
  expect_equal(gt[5], 0) # rotation y
  expect_equal(gt[6], -50) # pixel height (negative)

  mem_ds$close()
})

test_that("r_to_MEM validates input parameters correctly", {
  test_vec <- 1:100

  # Test negative dimensions
  expect_error(
    r_to_MEM(test_vec, nbands = -1, xsize = 10, ysize = 10),
    class = "vrtility_range_error"
  )

  expect_error(
    r_to_MEM(test_vec, nbands = 1, xsize = -10, ysize = 10),
    class = "vrtility_range_error"
  )

  # Test zero dimensions - this should also error
  expect_error(
    r_to_MEM(test_vec, nbands = 1, xsize = 10, ysize = 0)
  )

  # Test non-numeric srs
  expect_error(
    r_to_MEM(
      matrix(1:25, 5, 5),
      srs = 12345 # should be character
    ),
    class = "vrtility_type_error"
  )
})
