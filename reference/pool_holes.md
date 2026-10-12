# Get hole points for constrained triangulation

`pool_holes()` and `hole_points()` are the same function: `pool_holes()`
is the name that matches the other `pool_*()` accessors, and
`hole_points()` is kept for existing code.

## Usage

``` r
pool_holes(x, convention = c("sf", "ogc"))

hole_points(x, convention = c("sf", "ogc"))
```

## Arguments

- x:

  A wkpool (ideally after merge_coincident)

- convention:

  Which winding convention to use when the pool has no path provenance:
  "sf" (default) or "ogc"

## Value

A matrix of hole points, one point inside each hole cycle, or NULL if no
holes

## Details

For use with RTriangle::triangulate(). Each row is a point inside a
hole, which tells the triangulator to exclude that region.

The point is found on a horizontal line across the hole: the middle of
the widest stretch of that line that starts at the hole's boundary and
crosses no segment of the pool. It is inside the hole even when the hole
is not convex, where the centroid of the ring's vertices can fall
outside it (and a triangulator would then erase the polygon around the
hole instead), and it is clear of any island in the hole.

A hole that is exactly filled by another polygon in the pool (an enclave
in a coverage, such as a lake island that is its own feature) is not a
hole of the pool and is not returned.

When the pool carries path provenance, holes are identified structurally
(non-first rings of their part) and `convention` is not consulted.

## Examples

``` r
polygons_with_holes <- wk::as_wkb(c(
  paste0(
    "MULTIPOLYGON (((0 0, 0 1, 0.75 1, 1 0.8, 0.5 0.7, ",
    "0.8 0.6, 0.69 0, 0 0), (0.2 0.2, 0.5 0.2, ",
    "0.5 0.4, 0.3 0.6, 0.2 0.4, 0.2 0.2)))"),
  "MULTIPOLYGON (((0.69 0, 0.8 0.6, 1.1 0.63, 1.23 0.3, 0.69 0)))"
))

x <- establish_topology(polygons_with_holes)
merged <- merge_coincident(x)
pool_holes(merged)
#>          x   y
#> [1,] 0.325 0.5
if (requireNamespace("RTriangle", quietly = TRUE)) {
  pslg <- as_pslg(merged)
  holes <- pool_holes(merged)
  tri <- RTriangle::triangulate(
    RTriangle::pslg(P = pslg$P, S = pslg$S, H = holes)
  )
}
```
