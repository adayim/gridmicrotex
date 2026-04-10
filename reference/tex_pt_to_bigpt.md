# Convert TeX points to big points

TeX points (1/72.27 inch) differ from PostScript/big points (1/72 inch).
R's grid "bigpts" unit uses PostScript points.

## Usage

``` r
tex_pt_to_bigpt(tex_pt)
```

## Arguments

- tex_pt:

  Numeric value in TeX points.

## Value

Numeric value in big (PostScript) points.
