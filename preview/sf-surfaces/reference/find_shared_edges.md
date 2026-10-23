# Find shared edges between features

Find shared edges between features

## Usage

``` r
find_shared_edges(x)
```

## Arguments

- x:

  A wkpool (ideally after merge_coincident)

## Value

Data frame of edges shared by multiple features

## Details

After merging coincident vertices, edges that were duplicated across
features (e.g., shared polygon boundaries) will reference the same
vertex pair. This function finds them and reports which features share
each edge.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
merged <- merge_coincident(pool)
find_shared_edges(merged)
#>   edge_key .vx0 .vx1 .feature segment_idx features
#> 2      2-3    2    3        1           2     1, 2
#> 8      2-3    2    3        2           8     1, 2
```
