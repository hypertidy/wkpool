# Convert cycles to WKT polygons

Convert cycles to WKT polygons

## Usage

``` r
cycles_to_wkt(x, feature = TRUE, convention = c("sf", "ogc"))

cycles_to_wkb(x, feature = TRUE, convention = c("sf", "ogc"), ...)
```

## Arguments

- x:

  A wkpool (ideally after merge_coincident)

- feature:

  Logical: attempt to reconstruct original features? If TRUE, groups
  cycles by .feature and nests holes in outers. If FALSE, each cycle
  becomes a separate polygon.

- convention:

  Winding convention: "sf" (default) or "ogc"; only consulted for pools
  without path provenance

- ...:

  Passed to
  [`wk::as_wkb()`](https://paleolimbot.github.io/wk/reference/wkb.html)

## Value

A wk_wkt vector of POLYGON geometries

## Details

Converts cycles back to polygons. When feature = TRUE and the pool
carries path provenance (minted by
[`establish_topology()`](https://hypertidy.github.io/wkpool/reference/establish_topology.md)),
the original feature structure is reconstructed exactly: rings are
grouped into their original part (exterior first, then that part's
holes) and parts into their original feature (POLYGON or MULTIPOLYGON),
with winding emitted as stored and `convention` not consulted. Pools
without provenance fall back to a heuristic that groups rings by feature
and nests all holes with the first outer.

Geometry is assembled natively via
[`wk::wk_polygon()`](https://paleolimbot.github.io/wk/reference/wk_linestring.html)
and
[`wk::wk_collection()`](https://paleolimbot.github.io/wk/reference/wk_linestring.html)
(WKB first, WKT derived from it): coordinates are emitted at full double
precision, rings are closed by wk, z is carried when the pool has it,
and the pool's crs and geodesic flag are attached to the output.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
cycles_to_wkt(merged)
#> <wk_wkt[2]>
#> [1] POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0)) POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))
```
