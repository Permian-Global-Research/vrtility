# Mirai Daemon Management

Just some helpful functions for managing the mirai daemons and parallel
processing.

Load vrtility python environment in mirai daemons

## Usage

``` r
n_daemons()

daemons_load_vrtility(py_pkgs = "numpy", ...)
```

## Arguments

- py_pkgs:

  A character vector of python packages to ensure are loaded in the
  mirai daemons.

- ...:

  Additional arguments passed to
  [`vrtility_py_require`](https://permian-global-research.github.io/vrtility/reference/vrtility_python.md)

## Value

For `n_daemons()`, an integer indicating the number of active mirai
daemon connections. For `daemons_load_vrtility()`, `NULL` invisibly.

## Details

`n_daemons()` returns the number of mirai daemons running.

`daemons_load_vrtility()` loads the vrtility Python environment on all
mirai daemons. Called for its side effect; returns `NULL` invisibly.
