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
get_tiles <- function(img_rows, img_cols, x_window, y_window, overlap = 0) {
  # make sure input values are integers
  img_rows <- as.integer(img_rows)
  img_cols <- as.integer(img_cols)
  x_window <- as.integer(x_window)
  y_window <- as.integer(y_window)
  overlap <- as.integer(overlap)

  n <- ceiling((img_rows / x_window)) * ceiling((img_cols / y_window))

  x_vec <- y_vec <- integer(n)
  nXSize_vec <- nYSize_vec <- integer(n)

  i <- 1L
  for (x in seq.int(1L, img_rows, y_window)) {
    if (x + y_window + overlap <= img_rows) {
      nXSize <- y_window + overlap
    } else {
      nXSize <- img_rows - x + 1L
    }

    for (y in seq.int(1L, img_cols, x_window)) {
      if (y + x_window + overlap <= img_cols) {
        nYSize <- x_window + overlap
      } else {
        nYSize <- img_cols - y + 1L
      }

      x_vec[i] <- x
      y_vec[i] <- y
      nXSize_vec[i] <- nXSize
      nYSize_vec[i] <- nYSize
      i <- i + 1L
    }
  }

  mat <- matrix(c(x_vec, y_vec, nXSize_vec, nYSize_vec), ncol = 4)
  colnames(mat) <- c("nXOff", "nYOff", "nXSize", "nYSize")

  mat[, 1:2] <- mat[, 1:2] - 1L

  return(mat)
}


#' Aggregate raster blocks
#'
#' @param blocks_df A data frame of block definitions with columns nXOff, nYOff, nXSize, nYSize
#' @param max_size Maximum size in pixels for aggregated blocks (default 1024)
#' @param direction Direction of aggregation ("row" or "both")
#' @return A data frame of aggregated blocks
#' @examples
#' blocks_df <- get_tiles(1000, 1000, 128, 128)
#' # Aggregate blocks in rows
#' aggregate_blocks(blocks_df, max_size = 512)
#' @export
aggregate_blocks <- function(
  blocks_df,
  max_size = 1024,
  direction = "row",
  debug = FALSE
) {
  # Sort blocks by row (nYOff) then column (nXOff)
  blocks_df <- blocks_df[order(blocks_df$nYOff, blocks_df$nXOff), ]

  # Split blocks by row (same nYOff)

  row_groups <- split(blocks_df, blocks_df$nYOff)
  # browser()
  if (debug) browser()
  if (all(lapply(row_groups, nrow) == 1)) {
    row_groups <- list(blocks_df)
  }

  if (direction == "row") {
    adj_col <- "nXSize"
    alt_col <- "nYSize"
  } else {
    adj_col <- "nYSize"
    alt_col <- "nXSize"
  }

  # Aggregate blocks within each row
  aggregated_blocks <- do.call(
    rbind,
    lapply(row_groups, function(row_blocks) {
      # Initialize result for this row
      # browser()
      result_blocks <- data.frame()
      current_block <- row_blocks[1, ]
      current_size <- current_block[adj_col]
      ncell <- current_size * current_block[alt_col]

      for (i in 2:nrow(row_blocks)) {
        if (
          ncell + (row_blocks[i, adj_col] * row_blocks[i, alt_col]) <= max_size
        ) {
          # Merge with current block
          current_block[adj_col] <- current_block[adj_col] +
            row_blocks[i, adj_col]
          current_size <- current_block[adj_col]
          ncell <- current_size * current_block[alt_col]
        } else {
          # Add current block to results and start new block
          result_blocks <- rbind(result_blocks, current_block)
          current_block <- row_blocks[i, ]
          current_size <- current_block[adj_col]
          ncell <- current_size * current_block[alt_col]
        }
      }
      # Add final block
      rbind(result_blocks, current_block)
    })
  )

  rownames(aggregated_blocks) <- NULL
  aggregated_blocks
}
