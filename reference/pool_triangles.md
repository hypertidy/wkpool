# Access the triangles of a triangle pool

Access the triangles of a triangle pool

## Usage

``` r
pool_triangles(x)

tri_segments(x)

# S3 method for class 'wkpool_tri'
wk_handle(handleable, handler, ...)
```

## Arguments

- x:

  A `wkpool_tri`.

- handleable:

  A `wkpool_tri`.

- handler:

  A [wk
  handler](https://paleolimbot.github.io/wk/reference/wk_handle.html).

- ...:

  Passed on.

## Value

- `pool_triangles()`: a data frame with columns `.vx0`, `.vx1`, `.vx2`
  and, when present, `.feature` and `.part`.

- `tri_segments()`: a
  [`wkpool()`](https://hypertidy.github.io/wkpool/reference/wkpool.md)
  of the triangle edges, three per triangle in triangle order and
  counter-clockwise, on the same vertex pool, with the triangle's
  feature. Edges used by one triangle are on the boundary of the
  triangulated region, edges used by two are internal (after
  [`merge_coincident()`](https://hypertidy.github.io/wkpool/reference/merge_coincident.md),
  so that adjacent triangles share vertex ids).

## Details

A `wkpool_tri` is wk-handleable: it presents as one POLYGON per triangle
(the handled vector has the same length as the triangle vector), with
the pool's crs and geodesic flag attached.

## Examples

``` r
v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
tri <- new_wkpool_tri(v, vx0 = c(1L, 1L), vx1 = c(2L, 3L), vx2 = c(3L, 4L))
pool_triangles(tri)
#>   .vx0 .vx1 .vx2
#> 1    1    2    3
#> 2    1    3    4
tri_segments(tri)
#> <wkpool[6 segments, 4 vertices]>
#> [1] <segment: 1->2> <segment: 2->3> <segment: 3->1> <segment: 1->3>
#> [5] <segment: 3->4> <segment: 4->1>
```
