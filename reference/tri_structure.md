# Parts and holes of groups of triangles

The number of polygons (parts) and holes of each group, as
`ST_NumGeometries()` and `ST_NumInteriorRings()` (summed over parts)
give them for the dissolved groups. These need the boundary rings, so
they are a read on a pool from
[`tri_prepare()`](https://hypertidy.github.io/wkpool/reference/tri_prepare.md)
and a dissolve otherwise.

## Usage

``` r
tri_structure(x, group = NULL)
```

## Arguments

- x:

  A `wkpool_tri`.

- group:

  Integer group id per triangle (1-based, `NA` to leave a triangle out).
  The default is the triangles' feature, or one group when the pool has
  no feature.

## Value

A data frame with one row per group id from 1 to the largest and columns
`group`, `n_parts`, `n_holes` and `n_rings`.

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
