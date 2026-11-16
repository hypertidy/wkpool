# Read triangles from an Arrow array

Read an array written by
[`tri_as_arrow()`](https://hypertidy.github.io/wkpool/reference/tri_as_arrow.md)
(or anything with the same storage: one shared dictionary of `x`, `y`
coordinates indexed by fixed-size lists of three, optionally inside a
list per feature) back into a triangle pool and its groups.

## Usage

``` r
tri_from_arrow(x)
```

## Arguments

- x:

  A `nanoarrow_array`, or anything
  [`nanoarrow::as_nanoarrow_array()`](https://arrow.apache.org/nanoarrow/latest/r/reference/as_nanoarrow_array.html)
  accepts (such as an Arrow `Array`).

## Value

A list with `mesh`, a `wkpool_tri` (with the feature of each triangle
when every triangle is in one row), and `group`, a data frame of
`triangle` (row of `mesh`) and `group` (row of the array), or `NULL` for
the one-row-per-triangle layout.

## Details

A triangle that appears in several rows (a triangle in several features)
becomes one triangle of the pool, in several groups.

## Examples

``` r
if (requireNamespace("nanoarrow", quietly = TRUE)) {
  v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
  tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                        vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
  a <- tri_as_arrow(tri)
  a$length
  back <- tri_from_arrow(a)
  tri_measures(back$mesh, back$group)
}
#>   group area perimeter centroid_x centroid_y   point_x   point_y xmin ymin xmax
#> 1     1    1         4        0.5        0.5 0.6666667 0.3333333    0    0    1
#> 2     2    1         4        1.5        0.5 1.6666667 0.3333333    1    0    2
#>   ymax
#> 1    1
#> 2    1
```
