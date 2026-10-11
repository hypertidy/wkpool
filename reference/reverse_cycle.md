# Reverse a cycle's winding direction

Reverse a cycle's winding direction

## Usage

``` r
reverse_cycle(cycle)
```

## Arguments

- cycle:

  Integer vector of .vx IDs forming a closed cycle

## Value

The same vertices in reversed order (opposite winding)

## Details

Flips the traversal direction of a cycle without changing coordinates.
Useful for converting between winding conventions.

## Examples

``` r
x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
pool <- establish_topology(x)
merged <- merge_coincident(pool)
cycles <- find_cycles(merged)
reverse_cycle(cycles[[1]])
#> [1] 4 3 2 1
```
