% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial-utils.R
\name{to_projected}
\alias{to_projected}
\title{Project an bounding box to a generic projected coordinate system}
\usage{
to_projected(
  x,
  proj = c("laea", "aeqd", "utm", "pconic", "eqdc"),
  ellps = "WGS84",
  no_defs = TRUE,
  opts = "",
  return_as = c("wkt", "proj4")
)
}
\arguments{
\item{x}{numeric vector of length 4 representing a bounding box (in lat/long)}

\item{proj}{a character vector. The projection to use. One of "laea", "aeqd",
"utm", "pconic", or "eqdc".}

\item{ellps}{a character vector. The ellipsoid to use. Select from
\code{sf_proj_info(type = "ellps")}.}

\item{no_defs}{a logical. Whether to include the +no_defs option in the proj
string.}

\item{opts}{a character vector. Additional proj options to pass to the
proj string. see details for more information.}

\item{return_as}{a character vector. Whether to return the proj4 string or
WKT representation of the projected coordinate system.}
}
\value{
a character vector. The proj4 string or WKT representation of the
projected coordinate system.
}
\description{
Project an bounding box to a generic projected coordinate system
}
\details{
For further info about the available "generic" projects see:
for utm: \url{https://proj.org/en/9.4/operations/projections/utm.html}
for laea: \url{https://proj.org/en/9.4/operations/projections/laea.html}
for aeqd: \url{https://proj.org/en/9.4/operations/projections/aeqd.html}
for pconic: \url{https://proj.org/en/9.4/operations/projections/pconic.html}
for eqdc: \url{https://proj.org/en/9.4/operations/projections/eqdc.html}
}
