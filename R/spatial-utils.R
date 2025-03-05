#' Validate a bounding box
#' @param bbox A numeric vector of length 4 representing a bounding box
#' @return A numeric vector of length 4 representing a bounding box
#' @export
#' @examples
#' validate_bbox(c(-180, -90, 180, 90))
validate_bbox <- function(bbox) {
  v_assert_type(bbox, "bbox", "numeric")
  v_assert_length(bbox, "bbox", 4)

  bbox_poly <- bbox_to_wkt(bbox)

  if (!gdalraster::g_is_valid(bbox_poly)) {
    rlang::abort(
      "Bounding box (bbox) is not valid",
      class = "vrtility_bbox_not_valid"
    )
  }

  c_bbox <- gdalraster::bbox_from_wkt(bbox_poly)

  if (
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

#' Convert a bounding box to a WKT string
#' @param bbox A numeric vector of length 4 representing a bounding box
#' @return A WKT string
#' @keywords internal
#' @export
bbox_to_wkt <- function(bbox) {
  wk::wkt(
    glue::glue(
      "POLYGON (({bbox[1]} {bbox[2]},
      {bbox[3]} {bbox[2]}, {bbox[3]} {bbox[4]},
      {bbox[1]} {bbox[4]}, {bbox[1]} {bbox[2]}))"
    ),
    crs = "EPSG:4326"
  )
}


#' Project an bounding box to a generic projected coordinate system
#' @param x numeric vector of length 4 representing a bounding box (in lat/long)
#' @param proj a character vector. The projection to use. One of "laea", "aeqd",
#' "utm", "pconic", or "eqdc".
#' @param ellps a character vector. The ellipsoid to use. Select from
#' `sf_proj_info(type = "ellps")`.
#' @param no_defs a logical. Whether to include the +no_defs option in the proj
#' string.
#' @param opts a character vector. Additional proj options to pass to the
#' proj string. see details for more information.
#' @param return_as a character vector. Whether to return the proj4 string or
#' WKT representation of the projected coordinate system.
#' @return a character vector. The proj4 string or WKT representation of the
#' projected coordinate system.
#' @export
#' @details For further info about the available "generic" projects see:
#' for utm: \url{https://proj.org/en/9.4/operations/projections/utm.html}
#' for laea: \url{https://proj.org/en/9.4/operations/projections/laea.html}
#' for aeqd: \url{https://proj.org/en/9.4/operations/projections/aeqd.html}
#' for pconic: \url{https://proj.org/en/9.4/operations/projections/pconic.html}
#' for eqdc: \url{https://proj.org/en/9.4/operations/projections/eqdc.html}
to_projected <- function(
  x,
  proj = c("laea", "aeqd", "utm", "pconic", "eqdc"),
  ellps = "WGS84",
  no_defs = TRUE,
  opts = "",
  return_as = c("wkt", "proj4")
) {
  # arg assertions
  x <- validate_bbox(x)
  proj <- rlang::arg_match(proj)
  v_assert_type(no_defs, "no_defs", "logical")
  v_assert_type(opts, "opts", "character")
  return_as <- rlang::arg_match(return_as)

  # get centroid in latlong
  cent_coor <- gdalraster::g_centroid(bbox_to_wkt(x))

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

  switch(return_as, proj4 = prj, wkt = gdalraster::srs_to_wkt(prj))
}


#' Convert an object to wkt.
#' @param x The object to convert to wkt.
#' @return A character string of the object in wkt format.
#' @export
#' @rdname vrtility-internal
#' @keywords internal
to_wkt <- function(x) {
  UseMethod("to_wkt")
}

#' @rdname vrtility-internal
#' @export
to_wkt.default <- function(x) {
  rlang::abort(
    "to_wkt() not implemented for class {class(x)[1]}",
    class = "vrtility_to_wkt_not_implemented"
  )
}

#' @export
#' @rdname vrtility-internal
to_wkt.numeric <- function(x) {
  gdalraster::epsg_to_wkt(x)
}

#' @export
#' @rdname vrtility-internal
to_wkt.character <- function(x) {
  gdalraster::srs_to_wkt(x)
}

#TODO: we could consider extending to other crs classes in R spatial.
