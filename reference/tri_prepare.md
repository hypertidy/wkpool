# Prepare a triangle pool for repeated questions

Compute once, and keep with the pool, what the `tri_*()` functions
otherwise derive on every call: the triangles as rows of the vertex
table, the edge table
([`tri_neighbours()`](https://hypertidy.github.io/wkpool/reference/tri_neighbours.md)),
the membership of triangles in groups, and the boundary rings of each
group that
[`tri_dissolve()`](https://hypertidy.github.io/wkpool/reference/tri_dissolve.md)
and
[`tri_structure()`](https://hypertidy.github.io/wkpool/reference/tri_structure.md)
read. This is the mesh counterpart of a prepared geometry in GEOS.

## Usage

``` r
tri_prepare(x, group = NULL, rings = TRUE)

tri_is_prepared(x)
```

## Arguments

- x:

  A `wkpool_tri`.

- group:

  Groups of triangles. Either one integer per triangle (a dense, 1-based
  group id, `NA` to leave a triangle out), or a data frame with columns
  `triangle` (row in `x`) and `group`, for groups that overlap (a
  triangle in several groups, as in an overlay). The default is the
  triangles' feature, or one group when the pool has no feature.

- rings:

  Also compute the boundary rings of each group? Only for a `group`
  given per triangle (not a table of overlapping groups).

## Value

`x`, with the cache attached.

## Details

The cache is an attribute of the returned pool. It is used only while
the pool's vertices and triangles are unchanged (it is checked on every
use) and only for the `group` it was prepared with; any other `group` is
computed as usual. Subsetting or combining pools drops it.

## Examples

``` r
v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                      vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
p <- tri_prepare(tri)
tri_is_prepared(p)
#> [1] TRUE
tri_measures(p)
#>   group area perimeter centroid_x centroid_y   point_x   point_y xmin ymin xmax
#> 1     1    1         4        0.5        0.5 0.6666667 0.3333333    0    0    1
#> 2     2    1         4        1.5        0.5 1.6666667 0.3333333    1    0    2
#>   ymax
#> 1    1
#> 2    1
tri_structure(p)
#>   group n_parts n_holes n_rings
#> 1     1       1       0       1
#> 2     2       1       0       1
```
