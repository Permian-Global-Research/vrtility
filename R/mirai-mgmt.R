#' @title Mirai Daemon Management
#' @description Just some helpful functions for managing the mirai daemons and
#' parallel processing.
#' @rdname mirai-mgmt
#' @details
#' `n_daemons()` returns the number of mirai daemons running.
#' @export
n_daemons <- function() {
  mirai::status()$connections
}


#' @noRd
#' @keywords internal
#' @description command for setting up mirai daemons
daemon_setup <- function(gdal_config = NULL) {
  main_process_opts <- list(
    vrt.percent.ram = getOption("vrt.percent.ram"),
    vrt.pause.base = getOption("vrt.pause.base"),
    vrt.pause.cap = getOption("vrt.pause.cap"),
    vrt.max.times = getOption("vrt.max.times"),
    vrt.cache = getOption("vrt.cache")
  )

  mopts <- NULL

  if (mirai::daemons_set()) {
    evrywrs <- mirai::everywhere(
      {
        options(main_process_opts)
        if (!is.null(gdal_config)) {
          set_gdal_config(gdal_config)
        }

        gdalraster::set_cache_max(cache_max_val)

        # get options that where name begins with vrt.
        options()[grep("^vrt\\.", names(options()), value = TRUE)]
      },
      main_process_opts = main_process_opts,
      set_gdal_config = set_gdal_config,
      gdal_config = gdal_config,
      cache_max_val = gdalraster::get_cache_max("bytes")
    )
    # make sure we wait for options to be set
    mopts <- mirai::collect_mirai(evrywrs)
  }
  invisible(mopts)
}
