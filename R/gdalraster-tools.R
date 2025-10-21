#' Generate params from a raster file
#' @param x character path to the raster file
#' @param temp_vrt_dir character path to the temporary VRT directory
#' @param apply_scale logical indicating whether to apply the scale values
#' (if they exist) to the data
#' @return list containing the raster parameters
#' @keywords internal
#' @noRd
raster_template_params <- function(
  x,
  temp_vrt_dir = getOption("vrt.cache"),
  apply_scale = TRUE
) {
  vrt_template <- vrt_block_save_internal(
    x,
    temp_vrt_dir = temp_vrt_dir,
    apply_scale = apply_scale
  )
  tds <- methods::new(gdalraster::GDALRaster, vrt_template)
  on.exit(tds$close(), add = TRUE)
  xs <- tds$getRasterXSize()
  ys <- tds$getRasterYSize()
  nodataval <- tds$getNoDataValue(1)
  blksize <- tds$getBlockSize(1)
  nbands <- tds$getRasterCount()
  scale_vals <- purrr::map_vec(
    seq_len(nbands),
    ~ tds$getScale(.x)
  )
  offset_vals <- purrr::map_vec(
    seq_len(nbands),
    ~ tds$getOffset(.x)
  )
  data_type <- tds$getDataTypeName(1)

  return(list(
    vrt_template = vrt_template,
    xs = xs,
    ys = ys,
    nodataval = nodataval,
    blksize = blksize,
    nbands = nbands,
    scale_vals = scale_vals,
    data_type = data_type,
    offset_vals = offset_vals
  ))
}

#' Assertions for gdalraster engine functions.
#' @keywords internal
#' @noRd
gdalraster_engine_asserts_init <- function(
  outfile,
  config_options,
  creation_options,
  quiet,
  fname = "multiband_reduce"
) {
  v_assert_type(outfile, "outfile", "character", nullok = FALSE)
  v_assert_type(config_options, "config_options", "character", nullok = FALSE)
  v_assert_type(
    creation_options,
    "creation_options",
    "character",
    nullok = FALSE
  )
  v_assert_type(quiet, "quiet", "logical", nullok = FALSE)
}

#' Build a VSI source string for GDAL raster drivers
#' @param src character source path
#' @param vsi character VSI prefix (e.g., "/vsicurl/")
#' @param drive character GDAL driver prefix (e.g., "EOPFZARR")
#' @return character VSI source string
#' @keywords internal
#' @noRd
gdal_driver_vsi_src_builder <- function(src, vsi = "", drive = "") {
  osrc <- paste0(vsi, src)
  if (drive == "EOPFZARR") {
    parts <- strsplit(
      osrc,
      split = "(?<=\\.zarr)",
      perl = TRUE
    )
    osrc <- sapply(parts, function(x) {
      paste0(drive, ":", '"', x[1], '":', x[2])
    })
  } else if (nzchar(drive)) {
    osrc <- paste0(drive, ":", '"', osrc, '"')
  }

  return(osrc)
}

#' Create an in-memory GDAL raster dataset from R data
#' @param data numeric vector, matrix or array
#' @param nbands numeric number of bands
#' @param xsize numeric number of pixels in x direction
#' @param ysize numeric number of pixels in y direction
#' @param bbox numeric vector of length 4 representing the bounding box with
#' the order: xmin, ymin, xmax, ymax
#' @param srs character spatial reference system string
#' @return GDALRasterDataset object
#' @rdname gdalraster-helpers
#' @export
#' @examples
#' v2m <- vector_to_MEM(
#'   as.vector(c(
#'     sin(
#'       sqrt(outer(1:120 - 60, 1:110 - 55, function(x, y) x^2 + y^2)) * 0.9
#'     )
#'   )),
#'   nbands = 1,
#'   xsize = 120,
#'   ysize = 110
#' )
#'
#' v2m
#' plot(v2m)
vector_to_MEM <- function(
  data,
  nbands = NULL,
  xsize = NULL,
  ysize = NULL,
  bbox = NULL,
  srs = NULL
) {
  v_assert_type(
    data,
    "data",
    c("numeric", "integer", "matrix", "array"),
    nullok = FALSE,
    multiple = TRUE
  )
  purrr::map2(
    list(xsize, ysize, nbands),
    c("xsize", "ysize", "nbands"),
    function(.x, .y) {
      v_assert_type(
        .x,
        .y,
        c("numeric", "integer"),
        nullok = TRUE,
        multiple = TRUE
      )
      if (!is.null(.x)) {
        v_assert_within_range(.x, .y, 0, Inf)
        v_assert_length(.x, .y, 1)
      }
    }
  )

  v_assert_type(
    bbox,
    "bbox",
    c("numeric", "integer"),
    nullok = TRUE,
    multiple = TRUE
  )

  v_assert_type(srs, "srs", "character", nullok = TRUE)

  if (!is.null(attr(data, "gis"))) {
    gis <- attr(data, "gis")
    dims <- gis$dim
    bbox <- gis$bbox
    srs <- gis$srs
  } else {
    if (is.null(nbands)) {
      rlang::abort(
        "'nbands' must be specified if 'data' has no 'gis' attribute",
        class = "missing_dimensions_error"
      )
    }
    if (is.null(xsize) || is.null(ysize)) {
      rlang::abort(
        "'xsize' and 'ysize' must be specified if 'data' has no 'gis'attribute",
        class = "missing_dimensions_error"
      )
    }
    if (!is.null(bbox)) {
      validate_bbox(bbox, check_latlong = FALSE)
    } else {
      bbox <- c(0, 0, xsize, ysize)
    }
    dims <- c(xsize, ysize, nbands)
  }

  geo_trans <- c(
    bbox[1], # top left x
    (bbox[3] - bbox[1]) / dims[1], # pixel width
    0, # rotation x
    bbox[4], # top left y
    0, # rotation y
    -(bbox[4] - bbox[2]) / dims[2] # pixel height (negative)
  )
  nbands <- dims[3]

  memds <- gdalraster::create(
    "MEM",
    "",
    xsize = dims[1],
    ysize = dims[2],
    nbands = nbands,
    dataType = "Float32",
    return_obj = TRUE
  )
  if (!is.null(srs)) {
    memds$setProjection(srs)
  }
  memds$setGeoTransform(geo_trans)

  band_vals <- asplit(
    vals_to_array(data, dims = dims),
    MARGIN = 3
  )

  if (length(band_vals) < nbands) {
    band_vals <- c(
      band_vals,
      replicate(
        n = nbands - length(band_vals),
        array(NA_real_, dim = c(dims[1], dims[2])),
        simplify = FALSE
      )
    )
  }
  purrr::walk2(
    band_vals,
    seq_len(nbands),
    function(band_data, band_idx) {
      memds$write(
        band = band_idx,
        xoff = 0,
        yoff = 0,
        xsize = dims[1],
        ysize = dims[2],
        rasterData = band_data
      )
    }
  )
  return(memds)
}

vals_to_array <- function(r, dims = attributes(r)$gis$dim) {
  rows <- dims[1]
  cols <- dims[2]

  # Auto-determine number of bands
  pixels_per_band <- rows * cols
  total_length <- length(r)

  if (total_length %% pixels_per_band != 0) {
    cli::cli_abort(
      "Data length is not a multiple of spatial dimensions 
      {rows * cols}. Cannot reshape to array.",
      class = "invalid_data_length_error"
    )
  }

  nbands <- total_length / pixels_per_band

  array(r, dim = c(rows, cols, nbands))
}
