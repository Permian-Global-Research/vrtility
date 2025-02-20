v_assert_type <- function(
    x, name, type = c("character", "numeric", "logical", "function"),
    nullok = TRUE) {
  type <- rlang::arg_match(type)
  if (nullok && is.null(x)) {
    return()
  }
  if (!rlang::is_true(rlang::inherits_any(x, type))) {
    rlang::abort(
      "The '{name}' argument must be a character {type}",
      class = "vrtility_type_error"
    )
  }
}

v_assert_length <- function(x, name, length, nullok = TRUE) {
  if (nullok && is.null(x)) {
    return()
  }
  if (rlang::is_true(length(x) != length)) {
    rlang::abort("{name} must have length 4",
      class = "vrtility_length_error"
    )
  }
}
