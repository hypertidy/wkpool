# Combine wkpool objects

Combine wkpool objects

## Usage

``` r
pool_combine(...)
```

## Arguments

- ...:

  wkpool objects to combine

## Value

A single wkpool with merged pools and remapped segments

## Examples

``` r
x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
y <- wk::as_wkb("POLYGON ((2 0, 3 0, 3 1, 2 1, 2 0))")
pool_a <- establish_topology(x)
pool_b <- establish_topology(y)
pool_combine(pool_a, pool_b)
#> <wkpool[8 segments, 10 vertices]>
#> [1] <segment: 1->2>  <segment: 2->3>  <segment: 3->4>  <segment: 4->5> 
#> [5] <segment: 6->7>  <segment: 7->8>  <segment: 8->9>  <segment: 9->10>
```
