GITHUB_RAW_BASE <- paste0(
  "https://raw.githubusercontent.com/",
  "Permian-Global-Research/vrtility/main/vignettes/"
)

# After knitting, rewrite local figure/ paths to GitHub raw URLs so that
# images don't need to be bundled in the package tarball.
fix_image_paths <- function(filename) {
  rmd <- readLines(filename)
  # Matches: ![...](figure/...) and src="figure/..." and src='figure/...'
  rmd <- gsub(
    '(src=["\']|]\\()figure/',
    paste0("\\1", GITHUB_RAW_BASE, "figure/"),
    rmd
  )
  writeLines(rmd, filename)
}

prerender_it <- function(filename) {
  mirai::daemons(0)
  withr::with_dir("vignettes", {
    if (fs::file_exists(filename)) {
      fs::file_delete(filename)
    }
    knitr::knit(paste0(filename, ".orig"), filename)
    fix_image_paths(filename)
  })
}

prerender_it("cdse-sentinel-2.Rmd")

prerender_it("HLS.Rmd")
prerender_it("Digital-Earth-Africa-GeoMAD.Rmd")

prerender_it("OmniCloudMask.Rmd")

prerender_it("landsat.Rmd")

# prerender_it("EOPF.Rmd")

prerender_it("ndvi-timeseries.Rmd")

prerender_it("data-structures-and-terminology.Rmd")
