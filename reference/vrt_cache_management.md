# Set the VRT cache directory

Set the VRT cache directory

Destroy the VRT cache directory

## Usage

``` r
vrt_cache_set(dir)

vrt_cache_destroy()
```

## Arguments

- dir:

  A character string of the directory to set as the cache

## Value

A character string of the directory (invisibly)

## Details

The default cache location is
[`tempdir()`](https://rdrr.io/r/base/tempfile.html). This function
allows you to set the cache location to a different directory. It is
mainly useful for debugging or if you wish to interrogate intermediate
VRT files. `vrtility` depends on saving many vrt files to disk so a lot
of files can build up in the cache directory. temporary directory.

`vrt_cache_destroy` is a helper function that will destroy the VRT cache
directory. This is useful if you want to clear the cache directory and
start fresh. If the cache directory is set to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) then the function
will warn you that this is a bad idea and will not proceed.
