# Resolve MicroTeX FontStyle bitmask to R font face

MicroTeX FontStyle: rm=1, bf=2, it=4, etc. (bitmask).

## Usage

``` r
.resolve_text_face(style)
```

## Arguments

- style:

  Integer font style bitmask.

## Value

Character: `"plain"`, `"bold"`, `"italic"`, or `"bold.italic"`.
