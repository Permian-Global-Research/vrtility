#' Validate a bounding box
#' @param bbox A numeric vector of length 4 representing a bounding box
#' @return A numeric vector of length 4 representing a bounding box
#' @noRd
#' @keywords internal
validate_bbox <- function(bbox, check_latlong = TRUE) {
  v_assert_type(bbox, "bbox", "numeric")
  v_assert_length(bbox, "bbox", 4)

  bbox_poly <- gdalraster::bbox_to_wkt(bbox)

  if (!suppressWarnings(gdalraster::g_is_valid(bbox_poly))) {
    rlang::abort(
      "Bounding box (bbox) is not valid",
      class = "vrtility_bbox_not_valid"
    )
  }

  c_bbox <- gdalraster::bbox_from_wkt(bbox_poly)

  if (
    check_latlong &&
      !rlang::is_true(
        c_bbox[1] >= -180 &&
          c_bbox[3] <= 180 &&
          c_bbox[2] >= -90 &&
          c_bbox[4] <= 90
      )
  ) {
    rlang::abort(
      c(
        "Bounding box (bbox) is not within valid bounds",
        "i" = "Are you sure the bounding box is lat/long?"
      ),
      class = "vrtility_bbox_not_within_bounds"
    )
  }

  return(c_bbox)
}


#' Project a lat/long bounding box to a generic projected coordinate system
#' @description This function takes a lat/long bounding box and projects it to
#' either a prescibed projected coordinate system or a generic projected
#' coordinate system suitable for the AOI.
#' @param x numeric vector of length 4 representing a bounding box (in lat/long)
#' @param proj_specific a character vector. The projection to use.
#' A PROJ-readable string or an EPSG code. If NULL, a generic projection
#' will be used.
#' @param proj_generic a character vector. The projection to use. One of
#' "laea", "aeqd", "utm", "pconic", or "eqdc".
#' @param ellps a character vector. The ellipsoid to use. Select from
#' `sf_proj_info(type = "ellps")`.
#' @param no_defs a logical. Whether to include the +no_defs option in the proj
#' string.
#' @param opts a character vector. Additional proj options to pass to the
#' proj string. see details for more information.
#' @return a numeric vector of length 4 representing the projected bounding box
#' in the new coordinate system. Attributes include the new proj4 and wkt
#' string.
#' @export
#' @details For further info about the available "generic" projects see:
#' for utm: \url{https://proj.org/en/9.4/operations/projections/utm.html}
#' for laea: \url{https://proj.org/en/9.4/operations/projections/laea.html}
#' for aeqd: \url{https://proj.org/en/9.4/operations/projections/aeqd.html}
#' for pconic: \url{https://proj.org/en/9.4/operations/projections/pconic.html}
#' for eqdc: \url{https://proj.org/en/9.4/operations/projections/eqdc.html}
#' @examples
#' bbox <- gdalraster::bbox_from_wkt(
#'   wkt = "POINT (144.3 -7.6)",
#'   extend_x = 0.17,
#'   extend_y = 0.125
#' )
#'
#' bbox_to_projected(bbox)
#'
#' bbox_to_projected(bbox, proj_generic = "utm")
#'
#'
#' @examplesIf interactive()
#' bbox_to_projected(
#'  c(-3.56, 50.69, -3.46, 50.75),
#'  proj_specific = "EPSG:27700"
#' )
#' @rdname spatial_helpers
bbox_to_projected <- function(
  x,
  proj_specific = NULL,
  proj_generic = c("laea", "aeqd", "utm", "pconic", "eqdc"),
  ellps = "WGS84",
  no_defs = TRUE,
  opts = ""
) {
  # arg assertions
  x <- validate_bbox(x)

  if (!is.null(proj_specific)) {
    prj <- proj_specific

    if (!gdalraster::srs_is_projected(prj)) {
      cli::cli_abort(
        c(
          "x" = "`proj_specific` must be a valid projected coordinate system",
          "i" = "Use either a WKT / PROJ4 string or an EPSG code"
        ),
        class = "vrtility_proj_specific_not_valid"
      )
    }
  } else {
    proj <- rlang::arg_match(proj_generic)

    v_assert_type(no_defs, "no_defs", "logical")
    v_assert_type(opts, "opts", "character")

    # get centroid in latlong
    cent_coor <- gdalraster::g_centroid(gdalraster::bbox_to_wkt(x))

    # configure proj args
    n_or_s <- ifelse(
      cent_coor[2] == 0,
      "",
      ifelse(cent_coor[2] > 0, "+north", "+south")
    )

    no_defs <- ifelse(no_defs, "+no_defs", "")

    if (proj %in% c("pconic", "eqdc")) {
      lat_1 <- x[4]
      lat_2 <- x[2]
    }

    # construct proj4 string
    prj <- trimws(switch(
      proj,
      laea = glue::glue(
        "+proj=laea +lon_0={cent_coor[1]} +lat_0={cent_coor[2]}",
        "+ellps={ellps} {no_defs}",
        opts,
        .sep = " "
      ),
      utm = glue::glue(
        "+proj=utm +zone={round((180 + cent_coor[1]) / 6)} {n_or_s}",
        "+ellps={ellps} {no_defs}",
        opts,
        .sep = " "
      ),
      aeqd = glue::glue(
        "+proj=aeqd +lon_0={cent_coor[1]} +lat_0={cent_coor[2]}",
        "+ellps={ellps} {no_defs}",
        opts,
        .sep = " "
      ),
      pconic = glue::glue(
        "+proj=pconic +lon_0={cent_coor[1]} +lat_0={cent_coor[2]}",
        "+lat_1={lat_1} +lat_2={lat_2}",
        "+ellps={ellps} {no_defs}",
        opts,
        .sep = " "
      ),
      eqdc = glue::glue(
        "+proj=eqdc +lon_0={cent_coor[1]}",
        "+lat_1={lat_1} +lat_2={lat_2}",
        "+ellps={ellps} {no_defs}",
        opts,
        .sep = " "
      )
    ))
  }

  wkt <- gdalraster::srs_to_wkt(prj)

  te <- gdalraster::bbox_transform(
    x,
    gdalraster::srs_to_wkt("EPSG:4326"),
    wkt
  )
  attr(te, "wkt") <- wkt

  return(te)
}

#' Get bounding box from a spatial vector file
#' @param x character vector. Path to a spatial vector file.
#' @param latlon logical. Whether to return the bounding box in lat/long
#' (EPSG:4326). Default is FALSE.
#' @param extend_x numeric. Amount to extend the bounding box in the x
#' direction (both min and max).
#' @param extend_y numeric. Amount to extend the bounding box in the y
#' direction (both min and max).
#' @return A numeric vector of length 4 representing the bounding box ordered
#' as: xmin, ymin, xmax, ymax.
#' @export
#' @rdname spatial_helpers
ogr_bbox_from_file <- function(
  x,
  latlon = FALSE,
  extend_x = NULL,
  extend_y = NULL
) {
  v_assert_type(x, "x", "character")
  assert_files_exist(x, url_possible = TRUE)
  v_assert_type(latlon, "latlon", "logical", nullok = FALSE)

  vec <- methods::new(gdalraster::GDALVector, x)
  on.exit(vec$close())

  native_crs_bbox <- vec$bbox()

  if (!latlon) {
    return(extend_bbox(native_crs_bbox, extend_x, extend_y))
  }

  extend_bbox(
    gdalraster::transform_bounds(
      native_crs_bbox,
      vec$getSpatialRef(),
      "EPSG:4326"
    ),
    extend_x,
    extend_y
  )
}

#' Extend a bounding box by specified amounts in x and y directions
#' @param bbox A numeric vector of length 4 representing a bounding box
#' @param extend_x numeric. Amount to extend the bounding box in the x
#' direction (both min and max).
#' @param extend_y numeric. Amount to extend the bounding box in the y
#' direction (both min and max).
#' @return A numeric vector of length 4 representing the extended bounding box
#' @noRd
#' @keywords internal
extend_bbox <- function(bbox, extend_x = NULL, extend_y = NULL) {
  v_assert_type(bbox, "bbox", "numeric")
  v_assert_length(bbox, "bbox", 4)
  v_assert_type(extend_x, "extend_x", "numeric", nullok = TRUE)
  v_assert_length(extend_x, "extend_x", 1, nullok = TRUE)
  v_assert_type(extend_y, "extend_y", "numeric", nullok = TRUE)
  v_assert_length(extend_y, "extend_y", 1, nullok = TRUE)

  if (!is.null(extend_x)) {
    bbox[c(1, 3)] <- bbox[c(1, 3)] + c(-extend_x, extend_x)
  }
  if (!is.null(extend_y)) {
    bbox[c(2, 4)] <- bbox[c(2, 4)] + c(-extend_y, extend_y)
  }
  return(bbox)
}


#' Get the spatial reference system (SRS) from a spatial vector file
#' @param x character vector. Path to a spatial vector file.
#' @return A character string representing the SRS in WKT format.
#' @export
#' @rdname spatial_helpers
ogr_srs_from_file <- function(x) {
  v_assert_type(x, "x", "character")
  assert_files_exist(x, url_possible = TRUE)
  vec <- methods::new(gdalraster::GDALVector, x)
  on.exit(vec$close())
  vec$open(read_only = TRUE)
  vec$getSpatialRef()
}
