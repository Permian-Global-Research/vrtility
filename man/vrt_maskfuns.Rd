% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mask-pixel-funs.R
\name{set_mask_numpy}
\alias{set_mask_numpy}
\alias{build_intmask}
\alias{build_bitmask}
\title{Masking functions for pixel-based processing}
\usage{
set_mask_numpy()

build_intmask()

build_bitmask()
}
\description{
Masking functions for pixel-based processing
}
\details{
This function is a simple bit mask function that can be used to
mask out pixels based on a bit mask. There is no bitwise transformation
applied to the provided valid_bits.

build_intmask provides an integer mask function that can be used to
mask out pixels based on a bit mask. There is no bitwise transformation
applied to the provided valid_bits.

build_bitmask provides is a simple bit-wise mask function that can be used to
mask out pixels based on a bit mask. The valid_bits are used to determine
which bits to mask out.
}
