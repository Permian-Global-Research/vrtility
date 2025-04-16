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
#' @param n target number of chunks
#' @param direction Direction of aggregation ("row" or "both")
#' @return A data frame of aggregated blocks
#' @examples
#' blocks_df <- get_tiles(1000, 1000, 128, 128)
#' # Aggregate blocks in rows
#' aggregate_blocks(blocks_df, max_size = 512)
#' @export
#' @keywords internal
aggregate_blocks <- function(x, n) {
  row_blocks <- aggregate_blocks_internal(
    x,
    n_splits = n,
    direction = "row"
  )
  if (nrow(row_blocks) <= n) {
    return(row_blocks)
  }
  aggregate_blocks_internal(row_blocks, n_splits = n, direction = "col")
}

aggregate_blocks_internal <- function(
  blocks_df,
  n_splits = 4,
  direction = "row"
) {
  if (n_splits > 2) n_splits <- n_splits - 1

  max_size <- round(
    prod(
      max(blocks_df["nXOff"]) + tail(blocks_df["nXSize"], 1),
      max(blocks_df["nYOff"]) + tail(blocks_df["nYSize"], 1)
    ) /
      n_splits
  )

  # Sort blocks by row (nYOff) then column (nXOff)
  blocks_df <- blocks_df[order(blocks_df$nYOff, blocks_df$nXOff), ]

  # Split blocks by row (same nYOff)
  row_groups <- split(blocks_df, blocks_df$nYOff)

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
  aggregated_blocks <- purrr::map(row_groups, function(row_blocks) {
    # Initialize result for this row
    result_blocks <- data.frame()
    current_block <- row_blocks[1, ]
    current_size <- current_block[adj_col]
    ncell <- current_size * current_block[alt_col]

    if (nrow(row_blocks) > 1) {
      for (i in 2:nrow(row_blocks)) {
        next_size <- as.numeric(row_blocks[i, adj_col] * row_blocks[i, alt_col])

        # Add explicit NA check
        if (is.na(ncell) || is.na(next_size) || is.na(max_size)) {
          warning("NA values detected in block size calculations")
          next
        }

        if (ncell + next_size <= max_size) {
          # Merge with current block
          current_block[adj_col] <- as.numeric(current_block[adj_col]) +
            as.numeric(row_blocks[i, adj_col])
          current_size <- as.numeric(current_block[adj_col])
          ncell <- current_size * as.numeric(current_block[alt_col])
        } else {
          # Add current block to results and start new block
          result_blocks <- rbind(result_blocks, current_block)
          current_block <- row_blocks[i, ]
          current_size <- as.numeric(current_block[adj_col])
          ncell <- current_size * as.numeric(current_block[alt_col])
        }
      }
    }
    # Add final block
    rbind(result_blocks, current_block)
  }) |>
    purrr::list_rbind()

  rownames(aggregated_blocks) <- NULL
  aggregated_blocks
}

#' Suggest number of chunks based on available RAM
#' @param ys Number of rows in the raster
#' @param xs Number of columns in the raster
#' @param nbands Number of bands in the raster
#' @return Suggested number of chunks (numeric)
#' @noRd
#' @keywords internal
suggest_n_chunks <- function(ys, xs, nbands) {
  free_ram <- memuse::Sys.meminfo()$freeram
  estimated_ram <- memuse::howbig(
    nrow = ys * xs,
    ncol = 1,
    representation = "dense"
  ) *
    nbands *
    50

  max_av_ram <- getOption("vrt.percent.ram")

  avail_ram <- free_ram * max_av_ram / 100

  if (using_daemons()) {
    nprocs <- pmax(n_daemons(), 1)
  } else {
    nprocs <- 1
  }

  ceiling(1 / ((avail_ram / estimated_ram))) * nprocs
}
