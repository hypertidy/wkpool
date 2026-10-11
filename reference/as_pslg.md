# Convert wkpool to RTriangle pslg format

Convert wkpool to RTriangle pslg format

## Usage

``` r
as_pslg(x, ...)
```

## Arguments

- x:

  A wkpool (will be passed through merge_coincident if not already)

- ...:

  passed to merge_coincident

## Value

A list with P (vertex matrix) and S (segment matrix) for
RTriangle::pslg()

## Details

Produces a Planar Straight Line Graph suitable for constrained
triangulation with RTriangle. The vertex pool maps directly to P, and
segment indices map directly to S (both 1-indexed).

Note: Hole detection is not currently supported. The returned pslg does
not include hole points (H). For polygons with holes, you may need to
identify hole points manually using
[`find_cycles()`](https://hypertidy.github.io/wkpool/reference/find_cycles.md)
and
[`cycle_signed_area()`](https://hypertidy.github.io/wkpool/reference/cycle_signed_area.md).

## Examples

``` r
x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
pool <- establish_topology(x)
as_pslg(pool)
#> $P
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    1    0
#> [3,]    1    1
#> [4,]    0    1
#> 
#> $S
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    2    3
#> [3,]    3    4
#> [4,]    4    1
#> 
```
