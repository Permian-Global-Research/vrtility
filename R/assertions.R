v_assert_type <- function(
  x,
  name,
  type = c("character", "numeric", "logical", "function", "stac_vrt"),
  nullok = TRUE,
  multiple = FALSE
) {
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
    cli::cli_abort(
      "{name} must have length {length}",
      class = "vrtility_length_error"
    )
  }
}

v_assert_valid_schema <- function(x) {
  v_assert_true(
    fs::file_exists(x),
    "fs::file_exists(x)"
  )
  val_result <- xml2::xml_validate(xml2::read_xml(x), vrt_schema())
  if (!val_result) {
    error_msgs <- attr(val_result, "errors")
    cli::cli_abort(
      c(
        "!" = "Error when creating VRT block: invalid VRT XML:",
        purrr::map_chr(error_msgs, ~ cli::format_bullets_raw(c("x" = .x)))
      )
    )
  }
  invisible()
}

v_assert_true <- function(x, name) {
  if (!x) {
    cli::cli_abort(
      "'{name}' must be TRUE",
      class = "vrtility_true_error"
    )
  }
}

v_assert_false <- function(x, name) {
  if (x) {
    cli::cli_abort(
      "'{name}' must be FALSE",
      class = "vrtility_false_error"
    )
  }
}
