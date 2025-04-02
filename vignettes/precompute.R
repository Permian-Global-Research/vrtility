# Pre-compile vignettes
withr::with_dir("vignettes", {
  fs::file_delete("HLS.Rmd")
  knitr::knit("HLSo.Rmd.orig", "HLS.Rmd")
})
