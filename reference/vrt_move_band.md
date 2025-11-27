# Add an empty band to a VRT_x object

Add an empty band to a VRT_x object

## Usage

``` r
vrt_move_band(x, band_idx, after)
```

## Arguments

- x:

  A VRT_x object

- band_idx:

  numeric indicating the band number of the band to move.

- after:

  numeric indicating the band after which the new band should be places.
  Note this is based on the initial state of the band ordering. eg. do
  not add 1 if you are moving the band forward.
