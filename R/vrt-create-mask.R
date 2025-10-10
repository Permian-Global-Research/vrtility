#' Create a new mask for a vrt object
#' This function allows you to create a mask for a VRT object based on specified input bands and a mask function.
#' @param x A VRT_x object.
#' @param inbands A named numeric vector of input band indices to be used for
#' the mask.
#' @param maskfun A character containing a python pixel function to be applied
#' to the input bands to create the mask. Currently the only provided function
#' is \code{\link{create_omnicloudmask}}. This string must also have three key
#' attributes: `mask_name`, `mask_description`, and `required_bands`.
#' @param nodata_value The value to set as no data pixels
#' (numeric, default: 0).
#' @param cache_dir A character string specifying the directory to use for
#' caching temporary files. Default is the value of the `vrt.cache` option.
#' This should rarely need to be changed.
#' @return A VRT_x object with the the new mask band added.
#' @export
#' @rdname vrt_create_mask
#' @examplesIf interactive()
#'  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#'  ex_collect <- vrt_collect(s2files[4:5])
#'
#'  ex_mask <- vrt_create_mask(
#'    ex_collect,
#'    c(red = 3, green = 2, nir = 4),
#'    maskfun = create_omnicloudmask()
#'  )
#'
#'  ex_mask_compute <- vrt_compute(
#'    ex_mask,
#'    fs::file_temp(ext = ".tif"),
#'    recollect = TRUE
#'  )

#'  plot(ex_mask_compute, item = 2, bands =   6)
#'
vrt_create_mask <- function(
  x,
  inbands,
  maskfun,
  nodata_value = 0,
  cache_dir = getOption("vrt.cache")
) {
  UseMethod("vrt_create_mask")
}

#' @noRd
#' @keywords internal
#' @export
vrt_create_mask.default <- function(x, ...) {
  cli::cli_abort(
    "The vrt_create_mask method is not implemented for class {class(x)}",
    class = "vrtility_type_error"
  )
}

#' @export
vrt_create_mask.vrt_block <- function(
  x,
  inbands,
  maskfun,
  nodata_value = 0,
  cache_dir = getOption("vrt.cache")
) {
  v_assert_type(
    inbands,
    "inbands",
    c("numeric", "integer"),
    multiple = TRUE,
    nullok = FALSE
  )
  v_assert_is_named(inbands, "inbands")
  v_assert_type(maskfun, "maskfun", "character", nullok = FALSE)
  v_assert_create_mask_fun_attrs(
    maskfun,
    name = attributes(maskfun)$mask_name
  )
  v_assert_mask_names_match(inbands, maskfun)

  if (!all(names(inbands) %in% attributes(maskfun)$required_bands)) {
    cli::cli_inform(
      c(
        "The following bands are required by the {get_called_function_name(maskfun)} but are not 
      provided: {setdiff(attributes(maskfun)$required_bands, names(inbands))}"
      ),
      class = "vrtility_maskfun_error"
    )
  }

  # Reorder inbands based on required_bands order
  inbands <- inbands[attributes(maskfun)$required_bands]

  vx <- xml2::read_xml(x$vrt)

  bands <- xml2::xml_find_all(vx, ".//VRTRasterBand")
  # # Get the last band element
  last_band_index <- xml2::xml_find_num(
    vx,
    "count(.//VRTRasterBand[last()]/preceding-sibling::*) + 1"
  )

  msk_vrt_xml <- vrt_subset_bands(
    x$vrt_src,
    inbands,
    return_type = "xml",
    collapse = TRUE
  )

  msk_band <- xml2::xml_find_first(msk_vrt_xml, ".//VRTRasterBand")
  set_nodatavalue(msk_band, nodata_value)
  xml2::xml_set_attr(msk_band, "subClass", "VRTDerivedRasterBand")
  xml2::xml_add_child(msk_band, "PixelFunctionType", "create_mask")
  xml2::xml_add_child(msk_band, "PixelFunctionLanguage", "Python")

  cdata_node <- xml2::xml_cdata(maskfun)
  pixel_func_code <- xml2::xml_add_child(msk_band, "PixelFunctionCode")
  xml2::xml_add_child(pixel_func_code, cdata_node)
  xml2::xml_add_child(
    msk_band,
    "Description",
    attributes(maskfun)$mask_description
  )

  virt_mask_vrt <- fs::file_temp(tmp_dir = cache_dir, ext = "vrt")
  mat_mask_tif <- fs::file_temp(tmp_dir = cache_dir, ext = "tif")
  mat_mask_vrt <- fs::file_temp(tmp_dir = cache_dir, ext = "vrt")

  xml2::write_xml(msk_vrt_xml, virt_mask_vrt)

  ds <- new(gdalraster::GDALRaster, x$vrt_src)
  ds$getProjection()

  compute_with_py_env({
    call_gdal_warp(
      src_files = virt_mask_vrt,
      outfile = mat_mask_tif,
      t_srs = x$srs,
      cl_arg = combine_warp_opts(
        creation_options = gdal_creation_options(
          COPY_SRC_OVERVIEWS = "NO"
        ),
        warp_opts = gdalwarp_options(),
        resampling = "bilinear",
        te = x$bbox,
        res = x$res,
        dst_nodata = nodata_value
      ),
      config_options = gdal_config_opts(),
      quiet = TRUE
    )
  })

  gdalraster::translate(
    mat_mask_tif,
    mat_mask_vrt,
    quiet = TRUE
  )

  mat_mask_xml <- xml2::read_xml(mat_mask_vrt)
  mat_mask_rasband <- xml2::xml_find_first(mat_mask_xml, ".//VRTRasterBand")
  xml2::xml_set_attr(mat_mask_rasband, "band", length(bands) + 1)
  # drop_nodatavalue(mat_mask_rasband)

  xml2::xml_add_child(vx, mat_mask_rasband, .where = last_band_index)

  # Write back to block
  tf <- fs::file_temp(tmp_dir = cache_dir, ext = "vrt")
  xml2::write_xml(vx, tf)

  build_vrt_block(
    tf,
    maskfun = x$maskfun,
    pixfun = x$pixfun,
    warped = x$warped
  )
}

#' @export
vrt_create_mask.vrt_collection <- function(
  x,
  inbands,
  maskfun,
  nodata_value = 0,
  cache_dir = getOption("vrt.cache")
) {
  blocks_with_mask <- purrr::map(
    x$vrt,
    ~ vrt_create_mask(
      .x,
      inbands,
      maskfun,
      nodata_value = nodata_value,
      cache_dir = cache_dir
    )
  )

  if (inherits(x, "vrt_collection_warped")) {
    warped <- TRUE
  } else {
    warped <- FALSE
  }
  build_vrt_collection(
    blocks_with_mask,
    maskfun = x$maskfun,
    pixfun = x$pixfun,
    warped = warped
  )
}
