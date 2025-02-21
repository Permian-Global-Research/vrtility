v_assert_type <- function(
    x, name,
    type = c("character", "numeric", "logical", "function", "stac_vrt"),
    nullok = TRUE,
    multiple = FALSE) {
  type <- rlang::arg_match(type, multiple = multiple)
  if (nullok && is.null(x)) {
    return()
  }
  if (!rlang::is_true(rlang::inherits_any(x, type))) {
    cli::cli_abort(
      "The '{name}' argument must be a {type}",
      class = "vrtility_type_error"
    )
  }
}

v_assert_length <- function(x, name, length, nullok = TRUE) {
  if (nullok && is.null(x)) {
    return()
  }
  if (rlang::is_true(length(x) != length)) {
    cli::cli_abort("{name} must have length 4",
      class = "vrtility_length_error"
    )
  }
}
