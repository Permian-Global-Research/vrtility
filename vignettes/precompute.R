# Pre-compile vignettes
withr::with_dir("vignettes", {
  if (fs::file_exists("HLS.Rmd")) fs::file_delete("HLS.Rmd")
  knitr::knit("HLSo.Rmd.orig", "HLS.Rmd")
})
