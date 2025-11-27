#' function to optimise the tile reading
#' @description increases block size until the number of tiles is less than
#' the number of splits
#' @noRd
#' @keywords internal
optimise_tiling <- function(xs, ys, blksize, nsplits) {
  i <- 1
  # tile_matrix <- get_tiles(xs, ys, blksize[1], blksize[2])
  tile_matrix <- gdalraster::make_chunk_index(
    raster_xsize = xs,
    raster_ysize = ys,
    block_xsize = blksize[1],
    block_ysize = blksize[2],
    max_pixels = blksize[1] * blksize[2]
  )
  while (nrow(tile_matrix) > nsplits) {
    i <- i + 1
    tile_matrix <- gdalraster::make_chunk_index(
      raster_xsize = xs,
      raster_ysize = ys,
      block_xsize = blksize[1],
      block_ysize = blksize[2],
      max_pixels = blksize[1] * blksize[2] * i
    )
  }

  tile_matrix <- as.data.frame(tile_matrix)[, 3:6]
  colnames(tile_matrix) <- c("nXOff", "nYOff", "nXSize", "nYSize")
  return(tile_matrix)
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
