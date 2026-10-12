# Find internal boundaries (edges shared by exactly 2 features, opposite direction)

Find internal boundaries (edges shared by exactly 2 features, opposite
direction)

## Usage

``` r
find_internal_boundaries(x)
```

## Arguments

- x:

  A wkpool after merge_coincident

## Value

A wkpool containing only internal boundary segments

## Details

Internal boundaries are edges where one feature has segment (v0 -\> v1)
and another feature has (v1 -\> v0), i.e. they share the edge but
traverse it in opposite directions. This is the defining characteristic
of a shared polygon boundary.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
find_internal_boundaries(merged)
#> <wkpool[2 segments, 6 vertices]>
#> [1] <segment: 2->3> <segment: 3->2>
```
