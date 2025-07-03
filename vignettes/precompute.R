mirai::daemons(0)

# Pre-compile vignettes
withr::with_dir("vignettes", {
  if (fs::file_exists("HLS.Rmd")) {
    fs::file_delete("HLS.Rmd")
  }
  knitr::knit("HLSo.Rmd.orig", "HLS.Rmd")
})

# Pre-compile vignettes
withr::with_dir("vignettes", {
  if (fs::file_exists("Digital-Earth-Africa-GeoMAD.Rmd")) {
    fs::file_delete("Digital-Earth-Africa-GeoMAD.Rmd")
  }
  knitr::knit(
    "Digital-Earth-Africa-GeoMAD.Rmd.orig",
    "Digital-Earth-Africa-GeoMAD.Rmd"
  )
})
