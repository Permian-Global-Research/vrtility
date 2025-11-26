#' set the band names for a vrt_x object
#' @param x A vrt_block, vrt_stack, or vrt_collection object
#' @param band_names A character vector of the band names
#' @export
vrt_set_band_names <- function(x, band_names) {
  UseMethod("vrt_set_band_names")
}

#' @noRd
#' @export
vrt_set_band_names.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "!" = "{cli::code_highlight('vrt_set_band_names()')} not implemented for class {class(x)[1]}",
      "i" = "x must be a vrt_block, vrt_stack, or vrt_collection object."
    )
  )
}

#' @export
vrt_set_band_names.vrt_block <- function(x, band_names) {
  v_assert_type(band_names, "band_names", "character", nullok = FALSE)
  v_assert_length(band_names, "band_names", length(x$assets))
  tf <- vrt_save(x)
  of <- set_vrt_descriptions(tf, band_names, as_file = TRUE)

  build_vrt_block(
    x = of,
    maskfun = x$maskfun,
    pixfun = x$pixfun,
    is_remote = x$is_remote
  )
}

#' @export
vrt_set_band_names.vrt_stack <- function(x, band_names) {
  v_assert_type(band_names, "band_names", "character", nullok = FALSE)
  v_assert_length(band_names, "band_names", length(x$assets))
  tf <- vrt_save(x)
  of <- set_vrt_descriptions(tf, band_names, as_file = TRUE)

  if (inherits(x, "vrt_stack_warped")) {
    warped <- TRUE
  } else {
    warped <- FALSE
  }

  build_vrt_stack(
    of,
    n_items = x$n_items,
    maskfun = x$maskfun,
    pixfun = x$pixfun,
    warped = warped
  )
}

#' @export
vrt_set_band_names.vrt_collection <- function(x, band_names) {
  v_assert_type(band_names, "band_names", "character", nullok = FALSE)
  v_assert_length(band_names, "band_names", length(x$assets))
  block_list <- purrr::map(x[[1]], ~ vrt_set_band_names(.x, band_names))

  build_vrt_collection(block_list, maskfun = x$maskfun, pixfun = x$pixfun)
}
