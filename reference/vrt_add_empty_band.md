# Add an empty band to a VRT_x object

Add an empty band to a VRT_x object

## Usage

``` r
vrt_add_empty_band(x, after, description, scale_value)
```

## Arguments

- x:

  A VRT_x object

- after:

  numeric indicating the band number to add the empty band after

- description:

  A character string describing the empty band

- scale_value:

  A numeric value to set the scale of the new band. If NULL, the scale
  of the first band in the VRT will be used. Be careful, Landsat for
  example has different scales for different bands.
