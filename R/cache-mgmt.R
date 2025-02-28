#' Check if a path is in the temporary directory
#' @param path A character string of the path to check
#' @return A logical indicating if the path is in the temporary directory
is_temp_dir <- function(path) {
  # Get normalized paths
  norm_path <- fs::path_norm(path)
  norm_temp <- fs::path_norm(tempdir())

  # Check if path starts with temp directory path
  fs::path_has_parent(norm_path, norm_temp)
}


#' Set the VRT cache directory
#' @param dir A character string of the directory to set as the cache
#' @return A character string of the directory (invisibly)
#' @export
#' @rdname vrt_cache_management
#' @details
#' The default cache location is `tempdir()`. This function allows you to set
#' the cache location to a different directory. It is mainly useful if you
#' intend to work with parallel processing. `vrtility` depends on saving many
#' vrt files to disk and passing these files between processes fails when using a
#' temporary directory.
vrt_cache_set <- function(dir) {
  v_assert_type(dir, "dir", "character", nullok = FALSE)
  orig_cache <- options("vrt.cache")

  fs::dir_create(dir)
  options(vrt.cache = dir)

  if (dir == orig_cache) {
    cli::cli_alert_info(
      "Cache directory is already set to {cli::style_bold(dir)}"
    )
    return(invisible(dir))
  }

  cli::cli_alert_success(
    c(
      "Cache directory changed from {cli::style_bold(orig_cache)} to ",
      "{cli::style_bold(dir)}"
    )
  )

  invisible(dir)
}


#' Destroy the VRT cache directory
#' @return NULL
#' @export
#' @rdname vrt_cache_management
#' @details
#' A helper function that will destroy the VRT cache directory. This is useful
#' if you want to clear the cache directory and start fresh. If the cache
#' directory is set to `tempdir()` then the function will warn you that this is
#' a bad idea and will not proceed.
vrt_cache_destroy <- function() {
  dir <- options("vrt.cache")
  if (is_temp_dir(dir)) {
    cli::cli_warn(
      c(
        "!" = "Woah There! Your cache is set to {cli::code_highlight('tempdir()')}",
        " " = "Deleting the temporary directory is a bad idea!",
        " " = "Let's not do that..."
      )
    )
    return(invisible())
  }

  if (rlang::is_interactive()) {
    destroy_cache_menu <- nice_menu(
      "Are you sure you want to destroy the VRT cache directory at {dir}?",
      c("Yes", "No")
    )

    if (destroy_cache_menu == 2) {
      cli::cli_alert_warning("Cache directory destruction aborted")
      return(invisible())
    }
  } else {
    destroy_cache_menu <- 1
    cli::cli_warn(
      c(
        "!" = "Non-interactive session detected",
        " " = "Assuming you want to destroy the cache directory"
      )
    )
  }

  fs::dir_delete(fs::path(dir))

  cli::cli_alert_success(
    "Cache directory at {cli::style_bold(dir)} has been destroyed"
  )

  options(vrt.cache = tempdir())

  invisible()
}
