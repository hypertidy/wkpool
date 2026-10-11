# Classify cycles as outer rings or holes based on winding

Classify cycles as outer rings or holes based on winding

## Usage

``` r
classify_cycles(x, convention = c("sf", "ogc"))
```

## Arguments

- x:

  A wkpool (ideally after merge_coincident)

- convention:

  Which winding convention to use when the pool has no path provenance:
  "sf" (default) or "ogc"

  - sf: negative area = outer, positive = hole

  - ogc: positive area = outer, negative = hole

## Value

A data frame with cycle index, signed area, and type (outer/hole); when
path provenance is present, also `.path` and `.feature`

## Details

When the pool carries path provenance, ring roles are structural: the
first ring of each part is the exterior and later rings are its holes,
regardless of winding, and `convention` is not consulted. The signed
area still reports the observed winding. Pools without provenance are
classified by winding under the requested convention.

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
merged <- merge_coincident(pool)
classify_cycles(merged)
#>   cycle     area  type .path .feature
#> 1     1 -0.76200 outer     1        1
#> 2     2  0.09000  hole     2        1
#> 3     3 -0.19695 outer     3        2
```
