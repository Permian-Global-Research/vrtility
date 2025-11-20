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
    vrt.cache = getOption("vrt.cache"),
    vrt.add.pylibs = getOption("vrt.add.pylibs")
  )

  mopts <- NULL

  if (mirai::daemons_set()) {
    evrywrs <- mirai::everywhere(
      {
        options(main_process_opts)
        if (!is.null(gdal_config)) {
          set_gdal_config(gdal_config)
        }

        if (!is.null(main_process_opts$vrt.add.pylibs)) {
          vrtility_py_require(main_process_opts$vrt.add.pylibs)
        }

        gdalraster::set_cache_max(cache_max_val)

        # get options that where name begins with vrt.
        options()[grep("^vrt\\.", names(options()), value = TRUE)]
      },
      main_process_opts = main_process_opts,
      set_gdal_config = set_gdal_config,
      gdal_config = gdal_config,
      cache_max_val = gdalraster::get_cache_max("bytes"),
      vrtility_py_require = vrtility_py_require
    )
    # make sure we wait for options to be set
    mopts <- mirai::collect_mirai(evrywrs)
  }
  invisible(mopts)
}

#' @description Load vrtility python environment in mirai daemons
#' @param py_pkgs A character vector of python packages to ensure are loaded
#' in the mirai daemons.
#' @param ... Additional arguments passed to \code{\link{vrtility_py_require}}
#' @rdname mirai-mgmt
#' @export
daemons_load_vrtility <- function(py_pkgs = "numpy", ...) {
  if (mirai::daemons_set()) {
    evw <- mirai::everywhere(
      vrtility_py_require(packages = py_pkgs, ...),
      vrtility_py_require = vrtility_py_require
    )
    invisible(evw[])
  } else {
    cli::cli_alert_warning(
      "No mirai daemons are running - cannot load vrtility python environment."
    )
  }
  return(invisible())
}
