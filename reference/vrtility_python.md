# Setup the vrtility Python environment

A very thin wrapper around the
[`reticulate::py_install`](https://rstudio.github.io/reticulate/reference/py_install.html)
function to set up the necessary python environment and then set some
options required by the vrtility package. environment

set_py_env_vals sets the environment variables required by the vrtility
package - typically not required.

## Usage

``` r
vrtility_py_require(
  packages = NULL,
  python_version = ">=3.9",
  ...,
  exclude_newer = NULL,
  action = c("add", "remove", "set")
)

set_py_env_vals()

compute_with_py_env(code, config_options = NULL)
```

## Arguments

- packages:

  A character vector of Python packages to be available during the
  session. These can be simple package names like `"jax"` or names with
  version constraints like `"jax[cpu]>=0.5"`. Pip style syntax for
  installing from local files or a git repository is also supported (see
  details).

- python_version:

  A character vector of Python version constraints  
  (e.g., `"3.10"` or `">=3.9,<3.13"`).

- ...:

  Reserved for future extensions; must be empty.

- exclude_newer:

  Limit package versions to those published before a specified date.
  This offers a lightweight alternative to freezing package versions,
  helping guard against Python package updates that break a workflow.
  Accepts strings formatted as RFC 3339 timestamps (e.g.,
  `"2006-12-02T02:07:43Z"`) and local dates in the same format (e.g.,
  `"2006-12-02"`) in your system's configured time zone. Once
  `exclude_newer` is set, only the `set` action can override it.

- action:

  Determines how `py_require()` processes the provided requirements.
  Options are:

  - `"add"` (the default): Adds the entries to the current set of
    requirements.

  - `"remove"`: Removes *exact* matches from the requirements list.
    Requests to remove nonexistent entries are ignored. For example, if
    `"numpy==2.2.2"` is in the list, passing `"numpy"` with
    `action="remove"` will not remove it.

  - `"set"`: Clears all existing requirements and replaces them with the
    provided ones. Packages and the Python version can be set
    independently.

- code:

  The code to execute

- config_options:

  A named character vector of configuration options to set in the
  environment before executing the code. eg. generated from
  [`gdal_config_opts`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md)

## Value

Invisible

## Details

In general this function shouldnt be required but if, for example, you
wish to use a custom python pixel function which uses a python package
not installed by default (currently only numpy), then you can use this
function to install the package, making sure to set action as "add".

set_py_env_vals is only required if you are running reticulate::py_env
in some non-standard way. In general it is a lot easier to use \#
vrtility_py_require which will automatically set the necessary options.

## Examples

``` r
compute_with_py_env(print("Hello World"))
#> [1] "Hello World"
```
