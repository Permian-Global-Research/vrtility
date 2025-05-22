#' @title Mirai Daemon Management
#' @description Just some helpful functions for managing the mirai daemons and
#' parallel processing.
#' @details
#' `using_daemons()` returns `TRUE` if there are any mirai daemons running.
#' @rdname mirai-mgmt
#' @export
using_daemons <- function() {
  mirai::status()$connections > 0
}

#' @rdname mirai-mgmt
#' @details
#' `n_daemons()` returns the number of mirai daemons running.
#' @export
n_daemons <- function() {
  mirai::status()$connections
}


#' @rdname mirai-mgmt
#' @param all.tests	Logical: if true apply all known tests.
#' @param logical Logical: if possible, use the number of physical CPUs/cores
#' (if FALSE) or logical CPUs (if TRUE). Currently this is honoured only on
#' macOS, Solaris and Windows.
#' @details
#' `machine_cores()` returns the number of cores on the machine. This function
#' is from \code{\link[parallel]{detectCores}} and is used detect the number of
#' cores on the machine.
#' @export
machine_cores <- function(all.tests = FALSE, logical = TRUE) {
  systems <- list(
    linux = "grep \"^processor\" /proc/cpuinfo 2>/dev/null | wc -l",
    darwin = if (logical) "/usr/sbin/sysctl -n hw.logicalcpu 2>/dev/null" else
      "/usr/sbin/sysctl -n hw.physicalcpu 2>/dev/null",
    solaris = if (logical)
      "/usr/sbin/psrinfo -v | grep 'Status of.*processor' | wc -l" else
      "/bin/kstat -p -m cpu_info | grep :core_id | cut -f2 | uniq | wc -l",
    freebsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null",
    openbsd = "/sbin/sysctl -n hw.ncpuonline 2>/dev/null"
  )
  nm <- names(systems)
  m <- pmatch(nm, R.version$os)
  m <- nm[!is.na(m)]
  if (length(m)) {
    cmd <- systems[[m]]
    if (
      !is.null(
        a <- tryCatch(
          suppressWarnings(system(cmd, TRUE)),
          error = function(e) NULL
        )
      )
    ) {
      a <- gsub("^ +", "", a[1])
      if (grepl("^[1-9]", a)) return(as.integer(a))
    }
  }
  if (all.tests) {
    for (i in seq(systems))
      for (cmd in systems[i]) {
        if (
          is.null(
            a <- tryCatch(
              suppressWarnings(system(cmd, TRUE)),
              error = function(e) NULL
            )
          )
        )
          next
        a <- gsub("^ +", "", a[1])
        if (grepl("^[1-9]", a)) return(as.integer(a))
      }
  }
  NA_integer_
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

  if (using_daemons()) {
    evrywrs <- mirai::everywhere(
      {
        library(vrtility)
        options(main_process_opts)
        if (!is.null(gdal_config)) {
          set_gdal_config(gdal_config)
        }
        # get options that where name begins with vrt.
        options()[grep("^vrt\\.", names(options()), value = TRUE)]
      },
      main_process_opts = main_process_opts,
      gdal_config = gdal_config
    )
    mopts <- mirai::collect_mirai(evrywrs)
    purrr::walk(mopts, \(x) x, .parallel = TRUE)
  }
  invisible(mopts)
}
