test_that("check gdal works", {
  sysgdal_version <- as.numeric(
    strsplit(gdalraster::gdal_version()[4], "\\.")[[1]]
  )
  majv <- sysgdal_version[1]
  minv <- sysgdal_version[2]
  patchv <- sysgdal_version[3]

  expect_false(
    check_gdal_and_warn(maj_v_min = majv + 1, min_v_min = 0, patch_v_min = 0)
  )

  expect_false(
    check_gdal_and_warn(maj_v_min = majv, min_v_min = minv + 1, patch_v_min = 0)
  )

  expect_false(
    check_gdal_and_warn(
      maj_v_min = majv,
      min_v_min = minv,
      patch_v_min = patchv + 1
    )
  )

  expect_true(
    check_gdal_and_warn(
      maj_v_min = majv,
      min_v_min = minv,
      patch_v_min = patchv
    )
  )

  expect_true(
    check_gdal_and_warn(maj_v_min = 0, min_v_min = 0, patch_v_min = 1)
  )
})
