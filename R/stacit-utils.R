#' Validate that assets exist in a STAC collection
#' @param stac_source A STAC source URL (character string)
#' @param collection Collection ID to check
#' @param assets Character vector of asset names to validate
#' @return Character vector of available assets, or NULL if unable to determine
#' @noRd
#' @keywords internal
validate_collection_assets <- function(stac_source, collection, assets) {
  # Construct collection metadata URL
  collection_url <- paste0(stac_source, "/collections/", collection)

  # Fetch collection metadata
  collection_response <- try(
    {
      httr2::request(collection_url) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    },
    silent = TRUE
  )

  # Handle request errors
  if (inherits(collection_response, "try-error")) {
    cli::cli_warn(
      "Failed to fetch collection metadata for validation: {attr(collection_response, 'condition')$message}"
    )
    return(NULL) # Return NULL if we can't fetch metadata
  }

  # Extract available assets from item_assets or summaries
  available_assets <- character(0)

  # Try item_assets first (STAC 1.0+ standard)
  if (!is.null(collection_response$item_assets)) {
    available_assets <- names(collection_response$item_assets)
  }

  # Fallback to summaries if item_assets not available
  if (
    length(available_assets) == 0 && !is.null(collection_response$summaries)
  ) {
    # Some STAC APIs put asset info in summaries
    if (!is.null(collection_response$summaries$`eo:bands`)) {
      # Extract from eo:bands summaries
      bands_info <- collection_response$summaries$`eo:bands`
      if (is.list(bands_info) && length(bands_info) > 0) {
        available_assets <- purrr::map_chr(
          bands_info,
          ~ .x$name %||% .x$common_name %||% ""
        )
        available_assets <- available_assets[available_assets != ""]
      }
    }
  }

  # If we still don't have assets, return NULL
  if (length(available_assets) == 0) {
    cli::cli_inform(
      "Unable to determine available assets from collection metadata, skipping validation"
    )
    return(NULL)
  }

  # Return available assets for use with arg_match
  return(available_assets)
}


stacit_src <- function(
  stac_source,
  collection,
  bbox,
  start_date,
  end_date,
  assets,
  check = TRUE,
  quoted = FALSE # Set TRUE for shell commands, FALSE for gdalraster functions
) {
  if (check) {
    collection <- rlang::arg_match(
      collection,
      get_all_collections(
        stac_source,
        target_collection = collection
      )
    )
  }
  bbox <- validate_bbox(bbox, check_latlong = FALSE)
  bbox_str <- paste(bbox, collapse = ",")
  query_params <- glue::glue_collapse(
    c(
      glue::glue("collections={collection}"),
      glue::glue("bbox={bbox_str}"),
      glue::glue("datetime={start_date}T00:00:00Z%2F{end_date}T23:59:59Z")
    ),
    sep = "&"
  )

  # Validate assets exist in collection and get available assets
  if (check) {
    available_assets <- validate_collection_assets(
      stac_source,
      collection,
      assets
    )
    assets <- rlang::arg_match(assets, available_assets, multiple = TRUE)
  }

  # Create a STACIT URL for each asset
  purrr::map_chr(assets, function(asset) {
    asset_params <- glue::glue("asset={asset}")

    if (quoted) {
      # For shell commands - with escaped quotes
      glue::glue(
        '"STACIT:\\"{stac_source}/search?{query_params}\\":{asset_params}\\""'
      )
    } else {
      # For gdalraster functions - no outer quotes, no escaped inner quotes
      glue::glue(
        'STACIT:"{stac_source}/search?{query_params}":{asset_params}'
      )
    }
  })
}

# Helper function to create multi-band VRT from multiple assets
stacit_vrt <- function(
  vrt_filename,
  stac_source,
  collection,
  bbox,
  start_date,
  end_date,
  assets,
  check = TRUE
) {
  # Create STACIT sources for all assets
  input_rasters <- stacit_src(
    stac_source = stac_source,
    collection = collection,
    bbox = bbox,
    start_date = start_date,
    end_date = end_date,
    assets = assets,
    check = check,
    quoted = FALSE
  )

  # Build VRT with separate sources
  flist <- purrr::map(input_rasters, function(.x) {
    ds <- methods::new(
      gdalraster::GDALRaster,
      .x
    )
    browser()
    on.exit(ds$close())
    ds$getFileList()
  }) |>
    purrr::transpose() |>
    purrr::map(unlist)

  item_vrts <- purrr::map(flist, function(src) {
    ovrt <- fs::file_temp(ext = ".vrt")
    gdalraster::buildVRT(
      vrt_filename = ovrt,
      input_rasters = src,
      cl_arg = c("-separate"),
      quiet = TRUE
    )
    return(ovrt)
  }) |>
    purrr::list_c()

  return(item_vrts)
}
