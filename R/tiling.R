#' Specify parameters to load raster in blocks
#'
#' Helper function for specifying the block parameters (\code{nXOff},
#' \code{nYOff}, \code{nXsize}, and \code{nYSize}) required by \code{RasterIO}
#' argument in \link{read_stars}
#'
#' @param img_rows number of input raster rows (integer)
#' @param img_cols number of input raster columns (integer)
#' @param x_window number of rows in block (integer)
#' @param y_window number of columns in block (integer)
#' @param overlap number of overlapping pixels (integer)
#'
#' @return matrix with specified \code{nXOff}, \code{nYOff}, \code{nXsize},
#' and \code{nYSize} parameters for every block
#'
#' @examples
#' \dontrun{
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' r = read_stars(tif, proxy = TRUE)
#' tiles = st_tile(nrow(r), ncol(r), 256, 256)
#' for (i in seq_len(nrow(tiles))) {
#'   tile = read_stars(tif, proxy = FALSE, RasterIO = tiles[i, ])
#'   # write tiles to separate files
#'   write_stars(tile, dsn = paste0(i, ".tif"))
#' }
#' }
#' @noRd
#' @keywords internal
#' @details borrowed from stars package
get_tiles <- function(img_rows, img_cols, x_window, y_window, overlap = 0) {
  # make sure input values are integers
  img_rows <- as.integer(img_rows)
  img_cols <- as.integer(img_cols)
  x_window <- as.integer(x_window)
  y_window <- as.integer(y_window)
  overlap <- as.integer(overlap)

  # Vectorized approach - generate all x and y positions
  x_starts <- seq.int(1L, img_rows, y_window)
  y_starts <- seq.int(1L, img_cols, x_window)

  # Calculate sizes vectorized
  nXSize_vec <- ifelse(
    x_starts + y_window + overlap <= img_rows,
    y_window + overlap,
    img_rows - x_starts + 1L
  )

  nYSize_vec <- ifelse(
    y_starts + x_window + overlap <= img_cols,
    x_window + overlap,
    img_cols - y_starts + 1L
  )

  # Create all combinations using expand.grid (more efficient than nested loops)
  grid <- expand.grid(
    x_idx = seq_along(x_starts),
    y_idx = seq_along(y_starts),
    KEEP.OUT.ATTRS = FALSE
  )

  # Build result matrix
  mat <- matrix(
    c(
      x_starts[grid$x_idx],
      y_starts[grid$y_idx],
      nXSize_vec[grid$x_idx],
      nYSize_vec[grid$y_idx]
    ),
    ncol = 4
  )
  colnames(mat) <- c("nXOff", "nYOff", "nXSize", "nYSize")

  # Adjust offsets (0-based indexing for GDAL)
  mat[, 1:2] <- mat[, 1:2] - 1L

  return(mat)
}

#' function to optimise the tile reading
#' @description increases block size until the number of tiles is less than
#' the number of splits
#' @noRd
#' @keywords internal
optimise_tiling <- function(xs, ys, blksize, nsplits) {
  i <- 1
  tile_matrix <- get_tiles(xs, ys, blksize[1], blksize[2])
  while (nrow(tile_matrix) > nsplits) {
    i <- i + 1
    tile_matrix <- get_tiles(xs, ys, blksize[1] * i, blksize[2] * i)
  }
  return(as.data.frame(tile_matrix))
}


#' Suggest number of chunks based on available RAM
#' @param ys Number of rows in the raster
#' @param xs Number of columns in the raster
#' @param nbands Number of bands in the raster
#' @param nitems Number of items to process in each chunk
#' @param scalar A scalar to adjust the estimated RAM usage
#' @return Suggested number of chunks (numeric)
#' @noRd
#' @keywords internal
suggest_n_chunks <- function(ys, xs, nbands, nitems, scalar = 3) {
  ram_info <- memuse::Sys.meminfo()
  if (mirai::daemons_set()) {
    nprocs <- pmax(n_daemons(), 1)
  } else {
    nprocs <- 1
  }
  avail_ram <- ram_info$freeram * 0.7

  estimated_ram <- memuse::howbig(
    nrow = xs,
    ncol = ys,
    representation = "dense"
  ) *
    nbands *
    (nitems * scalar)

  ram_alloc_perc <- getOption("vrt.percent.ram")

  avail_ram <- avail_ram * ram_alloc_perc / 100

  ntiles <- ceiling(1 / ((avail_ram / estimated_ram))) * nprocs
  return(ntiles)
}
