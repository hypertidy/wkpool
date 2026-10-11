# Establish topology from any wk-handleable geometry

Establish topology from any wk-handleable geometry

## Usage

``` r
establish_topology(x, ...)
```

## Arguments

- x:

  Any geometry wk can handle (sf, sfc, wkb, wkt, geos, s2, xy, ...)

- ...:

  reserved for future use

## Value

A wkpool object (segments with vertex pool)

## Details

Vertices are minted as-is from the input coordinates. No snapping or
deduplication is performed - the pool represents the exact truth of the
input geometry. Use merge_coincident() afterwards to discover/establish
shared topology.

Segments track their feature origin via `.feature` attribute, enabling
discovery of shared boundaries and neighbour relations.

Segments also carry a minted `.path` id - one id per input ring or
linestring - with a sidecar table (see
[`pool_paths()`](https://hypertidy.github.io/wkpool/reference/wkpool-accessors.md))
recording each path's feature, part and ring identity as captured from
[`wk::wk_coords()`](https://paleolimbot.github.io/wk/reference/wk_vertices.html).
This provenance makes cycles, ring roles and feature structure
recoverable exactly, in input order and input winding: see
[`find_cycles()`](https://hypertidy.github.io/wkpool/reference/find_cycles.md),
[`classify_cycles()`](https://hypertidy.github.io/wkpool/reference/classify_cycles.md)
and
[`cycles_to_wkt()`](https://hypertidy.github.io/wkpool/reference/cycles_to_wkt.md).

The coordinate reference system of the input (as reported by
[`wk::wk_crs()`](https://paleolimbot.github.io/wk/reference/wk_crs.html))
and its geodesic flag
([`wk::wk_is_geodesic()`](https://paleolimbot.github.io/wk/reference/wk_is_geodesic.html))
are captured on the pool and travel with it through subsetting, merging
and compaction, and back out through the round-trip functions.

## Examples

``` r
x <- wk::as_wkb(c(
  paste0(
    "MULTIPOLYGON (((0 0, 0 1, 0.75 1, 1 0.8, 0.5 0.7, ",
    "0.8 0.6, 0.69 0, 0 0), (0.2 0.2, 0.5 0.2, ",
    "0.5 0.4, 0.3 0.6, 0.2 0.4, 0.2 0.2)))"),
  "MULTIPOLYGON (((0.69 0, 0.8 0.6, 1.1 0.63, 1.23 0.3, 0.69 0)))"
))

pool <- establish_topology(x)
```
