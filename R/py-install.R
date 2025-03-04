#' @title Install the Python environment for vrtility]
#' @description This function creates a virtual environment for the vrtility
#' package
#' @param pyenv The name or location of the virtual environment to create
#' @return Invisible
#' @export
#' @details This function creates a virtual environment for the vrtility package
#' and installs the required Python packages. It also sets the VRTILITY_PYTHON
#' environment variable to the name of the virtual environment. reticulate is
#' not called directly in this package but is used to manage the environment
#' that will be used by GDAL during pixel functions.
#' @rdname vrtility_python
build_vrtility_python <- function(pyenv = NULL) {
  if (is.null(pyenv)) {
    pyenviron <- "vrtility"
  } else {
    pyenviron <- pyenv
  }

  v_assert_type(pyenviron, "pyenv", "character")

  if (reticulate::virtualenv_exists(pyenviron)) {
    env_exists_choice <- nice_menu(
      "The virtual environment already exists. Do you want to recreate it?",
      choices = c("Yes", "No"),
      not_interactive = "{cli::code_highlight('build_vrtility_python()')}
        requires an interactive session."
    )

    if (env_exists_choice == 2) {
      return()
    } else {
      reticulate::virtualenv_remove(pyenviron)
    }
  }

  create_env_choice <- nice_menu(
    "Do you want to create the virtual environment?",
    choices = c("Yes", "No"),
    not_interactive = "{cli::code_highlight('build_vrtility_python()')}
      requires an interactive session."
  )

  if (create_env_choice == 2) {
    cli::cli_abort("Virtual environment creation aborted")
  }

  reticulate::virtualenv_create(pyenviron, packages = c("numba", "numpy"))

  reticulate::use_virtualenv(pyenviron, required = TRUE)

  if (reticulate::virtualenv_exists(pyenviron)) {
    cli::cli_alert_success(
      "The virtual environment has been created successfully"
    )
  } else {
    cli::cli_abort(
      "The virtual environment could not be created"
    )
  }

  if (reticulate::py_module_available("numba")) {
    cli::cli_alert_success(
      "The numba package has been installed successfully"
    )
  } else {
    cli::cli_abort(
      "The numba package could not be installed"
    )
  }

  set_py_env_vals(pyenviron)
  invisible()
}

#' @title Set Python environment variables
#' @description This function sets the environment variables required by the
#' vrtility package
#' @return Invisible
#' @keywords internal
#' @noRd
set_py_env_vals <- function(pyenviron) {
  Sys.setenv(VRTILITY_PYTHON = pyenviron)
  sys <- reticulate::import("sys")
  Sys.setenv(VRTILITY_PY_EXECUTABLE = sys$executable)
  Sys.setenv(VRTILITY_PY_VERSION_MAJOR = sys$version_info$major)
  Sys.setenv(VRTILITY_PY_VERSION_MINOR = sys$version_info$minor)
  invisible()
}

#' @title Install Python packages in the vrtility environment
#' @description This function installs Python packages in the vrtility
#' environment
#' @param pkgs A character vector of package names
#' @return Invisible
#' @export
#' @rdname vrtility_python
vrtility_python_pkg_install <- function(pkgs) {
  reticulate::py_install(
    pkgs,
    envname = Sys.getenv("VRTILITY_PYTHON"),
    method = "virtualenv"
  )
  invisible()
}
