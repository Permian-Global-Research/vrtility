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
    "vrt_collection",
    "vrt_block",
    "doc_items",
    "formula",
    "matrix",
    "array"
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

v_assert_length <- function(x, name, leng, nullok = TRUE) {
  if (nullok && is.null(x)) {
    return()
  }
  if (rlang::is_true(length(x) != leng)) {
    cli::cli_abort(
      c(
        "{name} must have length {leng}",
        "i" = "Got value with length {length(x)}: {.val {utils::head(x, 3)}}{if (length(x) > 3) '...' else ''}"
      ),
      class = "vrtility_length_error"
    )
  }
}

v_assert_length_gt <- function(x, name, length, nullok = TRUE) {
  if (nullok && is.null(x)) {
    return()
  }
  if (rlang::is_true(length(x) <= length)) {
    cli::cli_abort(
      c(
        "{name} must have length greater than {length}",
        "i" = "Got value with length {length(x)}"
      ),
      class = "vrtility_length_error"
    )
  }
}

v_assert_valid_schema <- function(x) {
  v_assert_true(
    fs::file_exists(x),
    "fs::file_exists(x)"
  )
  val_result <- xml2::xml_validate(xml2::read_xml(x), get_cached_vrt_schema())
  if (!val_result) {
    error_msgs <- attr(val_result, "errors")

    cli::cli_warn(
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


assert_files_exist <- function(x, url_possible = FALSE) {
  check_uri <- function(.x) {
    if (url_possible) {
      # handling urls - I don't think we want to validate the url...
      if (assert_is_url(.x)) {
        return(TRUE)
      }
    }
    chkpths <- fs::file_exists(.x)
    if (!all(chkpths)) {
      return(FALSE)
    }

    return(TRUE)
  }

  filechecks <- purrr::map_lgl(
    x,
    check_uri
  ) |>
    purrr::set_names(x)

  if (!all(filechecks)) {
    cli::cli_abort(
      c(
        "The following paths could not be located:",
        purrr::map_chr(
          names(which(!filechecks)),
          ~ cli::format_bullets_raw(c(">" = .x))
        )
      )
    )
  }
  invisible(unname(x))
}


assert_is_url <- function(path) {
  # Original pattern for standard URLs and VSI paths
  standard_pattern <- paste0(
    "(http|https|ftp|ftps|s3|gs)://",
    "|vsicurl|vsis3|vsigs|vsiaz|vsioss|vsiswift"
  )
  # This will throw an error if the URL is invalid
  grepl(standard_pattern, path)
}


v_assert_is_named <- function(x, name) {
  named <- rlang::is_named2(x)
  if (!named) {
    cli::cli_abort(
      "'{name}' must be a named vector",
      class = "vrtility_named_error"
    )
  }
}

v_assert_create_mask_fun_attrs <- function(x, name) {
  check_attr <- function(attr_name) {
    if (!attr_name %in% names(attributes(x))) {
      cli::cli_abort(
        c(
          "The provided mask function {name} does not have the required 
          attribute '{attr_name}'.",
          "i" = "Ensure that the mask function is created with the correct 
          attributes."
        ),
        class = "vrtility_maskcreator_fun_error"
      )
    }
  }
  check_attr("mask_name")
  check_attr("required_bands")
  check_attr("mask_description")
  return(invisible())
}


v_assert_mask_names_match <- function(inbands, maskfun) {
  if (!all(names(inbands) %in% attributes(maskfun)$required_bands)) {
    cli::cli_abort(
      c(
        "The following bands are required by the {attributes(maskfun)$mask_name} but are not 
      provided: {setdiff(attributes(maskfun)$required_bands, names(inbands))}"
      ),
      class = "vrtility_maskfun_error"
    )
  }
}


v_assert_formula_valid <- function(formlist) {
  lhs_names <- purrr::map_chr(formlist, function(f) {
    deparse(rlang::f_lhs(f))
  })

  # assert lengths 1
  purrr::walk(
    lhs_names,
    ~ v_assert_length(.x, "band formula lhs", 1, nullok = FALSE)
  )

  rhs_form <- purrr::map(formlist, ~ all.vars(rlang::f_rhs(.x)))

  purrr::walk(
    rhs_form,
    ~ v_assert_length_gt(.x, "band formula variables", 0, nullok = FALSE)
  )

  names(formlist) <- lhs_names
  return(formlist)
}


v_assert_blosc <- function(level = c("warn", "abort")) {
  level <- rlang::arg_match(level)
  act_fun <- switch(
    level,
    warn = cli::cli_warn,
    abort = cli::cli_abort
  )

  if (!check_blosc()) {
    act_fun(c(
      "!" = "Blosc compression is not available.",
      "i" = "GDAL built against Blosc is required for reading the EOPF ZARR
      format."
    ))
  }
}

v_assert_muparser <- function() {
  if (!check_muparser()) {
    cli::cli_abort(c(
      "!" = "muparser is not available.",
      "i" = "GDAL built with muparser support is required for
      `vrt_derived_block()`."
    ))
  }
}


v_asset_hls_catalog <- function(stac_source, collection) {
  if (stac_source == "https://planetarycomputer.microsoft.com/api/stac/v1/") {
    collection <- if (!collection %in% c("hls2-s30", "hls2-l30")) {
      cli::cli_abort(c(
        "When using the Planetary Computer STAC source,",
        "the HLS collection must be specified as one of 
        {.val hls2-s30} or {.val hls2-l30}"
      ))
    }
  } else {
    collection <- if (!collection %in% c("HLSS30_2.0", "HLSL30_2.0")) {
      cli::cli_abort(c(
        "When using the NASA EARTHDATA STAC source,",
        "the HLS collection must be specified as one of 
        {.val HLSS30_2.0} or {.val HLSL30_2.0}"
      ))
    }
  }
}


muparser_mask_warn <- function(func) {
  cli::cli_warn(
    c(
      "!" = "GDAL muparser support not available and/or GDAL version is < 3.12.0. ",
      "i" = "Cannot use muparser for `{func}`.",
      ">" = "Using Python implementation instead."
    ),
    class = "muparser_not_available_error"
  )
}
