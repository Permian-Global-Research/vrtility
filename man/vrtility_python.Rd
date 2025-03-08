% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/py-install.R
\name{build_vrtility_python}
\alias{build_vrtility_python}
\alias{vrtility_python_pkg_install}
\title{Install the Python environment for vrtility]}
\usage{
build_vrtility_python(pyenv = NULL)

vrtility_python_pkg_install(pkgs)
}
\arguments{
\item{pyenv}{The name or location of the virtual environment to create}

\item{pkgs}{A character vector of package names}
}
\value{
Invisible

Invisible
}
\description{
This function creates a virtual environment for the vrtility
package

This function installs Python packages in the vrtility
environment
}
\details{
This function creates a virtual environment for the vrtility package
and installs the required Python packages. It also sets the VRTILITY_PYTHON
environment variable to the name of the virtual environment. reticulate is
not called directly in this package but is used to manage the environment
that will be used by GDAL during pixel functions.
}
