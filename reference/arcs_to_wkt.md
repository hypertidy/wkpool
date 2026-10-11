# Convert arcs to WKT linestrings

Convert arcs to WKT linestrings

## Usage

``` r
arcs_to_wkb(x, quotient = FALSE, ...)

arcs_to_wkt(x, quotient = FALSE)
```

## Arguments

- x:

  A wkpool (ideally after merge_coincident)

- quotient:

  Walk the quotient graph: duplicated shared boundaries collapse to a
  single undirected edge (first occurrence keeps its direction), so a
  boundary shared by two features becomes ONE arc rather than one per
  feature, and its interior vertices are degree-2 pass-throughs. This is
  the TopoJSON-style arc decomposition of a polygon layer. Default FALSE
  walks every segment.

- ...:

  Passed to
  [`wk::as_wkb()`](https://paleolimbot.github.io/wk/reference/wkb.html)

## Value

A wk_wkt vector of LINESTRING geometries

## Details

Each arc (maximal segment sequence between nodes) becomes a linestring.

Geometry is assembled natively via
[`wk::wk_linestring()`](https://paleolimbot.github.io/wk/reference/wk_linestring.html)
(WKB first, WKT derived from it): coordinates are emitted at full double
precision, z is carried when the pool has it, and the pool's crs and
geodesic flag are attached to the output.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
arcs_to_wkt(merged)
#> <wk_wkt[4]>
#> [1] LINESTRING (1 0, 0 0, 0 1, 1 1) LINESTRING (1 0, 1 1)          
#> [3] LINESTRING (1 0, 2 0, 2 1, 1 1) LINESTRING (1 0, 1 1)          
```
