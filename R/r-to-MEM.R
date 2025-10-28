#' Create an in-memory GDAL raster dataset from R data
#' @param data numeric vector, matrix or array
#' @param nbands numeric number of bands
#' @param xsize numeric number of pixels in x direction
#' @param ysize numeric number of pixels in y direction
#' @param bbox numeric vector of length 4 representing the bounding box with
#' the order: xmin, ymin, xmax, ymax
#' @param srs character spatial reference system string
#' @param data_type character GDAL data type name (e.g., "Float32", "Int16"),
#' if NULL (default) and the data has no `gis` attribute, "Float32" is used.
#' @param no_data_value numeric NoData value to set for all bands. If NULL (default)
#' a sensible default NoData value for the specified `data_type` is used.
#' @return GDALRasterDataset object
#' @rdname gdalraster-helpers
#' @export
#' @details
#' If `data` has been generated from \code{\link[gdalraster]{read_ds}} it will
#' include a `gis` attribute containing the necessary spatial metadata. If this
#' is the case, the spatial parameters are automatically extracted from the
#' `gis` attribute.
#' @examples
#' v2m <- r_to_MEM(
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
r_to_MEM <- function(
  data,
  bbox,
  srs,
  data_type,
  no_data_value,
  nbands,
  xsize,
  ysize
) {
  UseMethod("r_to_MEM")
}

#' @export
#' @noRd
r_to_MEM.default <- function(
  data,
  ...
) {
  rlang::abort(
    paste0(
      "No 'r_to_MEM' method for object of class '",
      class(data)[1],
      "'"
    ),
    class = "invalid_r_to_MEM_data_type_error"
  )
}

#' @export
r_to_MEM.numeric <- function(
  data,
  bbox = NULL,
  srs = NULL,
  data_type = NULL,
  no_data_value = NULL,
  nbands = NULL,
  xsize = NULL,
  ysize = NULL
) {
  r_to_MEM_general(
    data = data,
    nbands = nbands,
    xsize = xsize,
    ysize = ysize,
    bbox = bbox,
    srs = srs,
    data_type = data_type,
    no_data_value = no_data_value
  )
}
#' @export
r_to_MEM.matrix <- function(
  data,
  bbox = NULL,
  srs = NULL,
  data_type = NULL,
  no_data_value = NULL,
  ...
) {
  r_to_MEM_general(
    data = data,
    nbands = 1,
    xsize = nrow(data),
    ysize = ncol(data),
    bbox = bbox,
    srs = srs,
    data_type = data_type,
    no_data_value = no_data_value
  )
}
#' @export
r_to_MEM.array <- function(
  data,
  bbox = NULL,
  srs = NULL,
  data_type = NULL,
  no_data_value = NULL,
  ...
) {
  dims <- dim(data)
  r_to_MEM_general(
    data = data,
    nbands = dims[3],
    xsize = dims[1],
    ysize = dims[2],
    bbox = bbox,
    srs = srs,
    data_type = data_type,
    no_data_value = no_data_value
  )
}

#' General implementation for r_to_MEM
#' @keywords internal
#' @noRd
r_to_MEM_general <- function(
  data,
  nbands = NULL,
  xsize = NULL,
  ysize = NULL,
  bbox = NULL,
  srs = NULL,
  data_type = NULL,
  no_data_value = NULL
) {
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
    data_type <- purrr::reduce(gis$datatype, gdalraster::dt_union)
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

    if (is.null(data_type)) {
      data_type <- "Float32"
    }
  }
  # set no data
  if (is.null(no_data_value)) {
    no_data_value <- get_nodata_value(data_type)
    if (!is.na(no_data_value)) {
      data[is.na(data)] <- no_data_value
    }
  }

  # Construct geo transform
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
    dataType = data_type,
    return_obj = TRUE
  )
  if (!is.null(srs)) {
    memds$setProjection(srs)
  }
  memds$setGeoTransform(geo_trans)
  purrr::walk(seq_len(nbands), function(band_idx) {
    memds$setNoDataValue(
      band = band_idx,
      noDataValue = no_data_value
    )
  })

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
