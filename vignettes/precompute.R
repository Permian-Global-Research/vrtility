prerender_it <- function(filename) {
  mirai::daemons(0)
  withr::with_dir("vignettes", {
    if (fs::file_exists(filename)) {
      fs::file_delete(filename)
    }
    knitr::knit(paste0(filename, ".orig"), filename)
  })
}

prerender_it("HLS.Rmd")
prerender_it("Digital-Earth-Africa-GeoMAD.Rmd")
prerender_it("cdse-sentinel-2.Rmd")
prerender_it("OmniCloudMask.Rmd")
