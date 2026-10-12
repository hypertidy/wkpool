# Convert wkpool to a PSLG (planar straight line graph)

Convert wkpool to a PSLG (planar straight line graph)

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

A list with P (vertex matrix) and S (segment matrix), the form used by
RTriangle::pslg()

## Details

Produces a planar straight line graph for constrained Delaunay
triangulation. The vertex pool maps directly to P, and segment indices
map directly to S (both 1-indexed). Shared boundaries appear once per
feature.

A PSLG does not record which side of a segment is inside. For RTriangle,
pass hole points from
[`pool_holes()`](https://hypertidy.github.io/wkpool/reference/pool_holes.md)
as `H`. The triangles come back as vertex and index matrices, and
[`new_wkpool_tri()`](https://hypertidy.github.io/wkpool/reference/new_wkpool_tri.md)
puts them on a vertex pool again; see
[`vignette("triangulation", package = "wkpool")`](https://hypertidy.github.io/wkpool/articles/triangulation.md).

For ear cutting that keeps the feature of each triangle, see
[`pool_triangulate()`](https://hypertidy.github.io/wkpool/reference/pool_triangulate.md)
(experimental).

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
