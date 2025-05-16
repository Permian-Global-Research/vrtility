v_assert_type <- function(
  x,
  name,
  type = c(
    "character",
    "numeric",
    "integer",
    "logical",
    "function",
    "stac_vrt",
    "vrt_collection"
  ),
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
        "x" = error_msgs
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

v_assert_res <- function(x) {
  v_assert_type(x, "tr", "numeric", nullok = FALSE, multiple = TRUE)
  if (length(x) == 1) {
    x <- c(x, x)
  }
  if (length(x) != 2) {
    cli::cli_abort(
      "tr must have length 1 or 2",
      class = "vrtility_length_error"
    )
  }
  return(x)
}

v_assert_within_range <- function(x, name, lwr, upr) {
  if (any(x < lwr) || any(x > upr)) {
    cli::cli_abort(
      c(
        "{name} must be within the range {lwr} to {upr}",
        "i" = "The value(s) {x} are outside of the range."
      ),
      class = "vrtility_range_error"
    )
  }
}


assert_srs_len <- function(x) {
  if (length(x$srs) > 1) {
    cli::cli_abort(
      c(
        "The {class(x)[1]} has {length(x$srs)} spatial reference systems but 
        must have a single projection.",
        "i" = "use `vrt_warp()` to unify the projection of the {class(x)[1]}."
      )
    )
  }
}


assert_files_exist <- function(x) {
  chkpths <- fs::file_exists(x)
  if (!all(chkpths)) {
    cli::cli_abort(
      c(
        "The following paths could not be located:",
        purrr::map_chr(
          names(which(!chkpths)),
          ~ cli::format_bullets_raw(c(">" = .x))
        )
      )
    )
  }
  invisible(x)
}
