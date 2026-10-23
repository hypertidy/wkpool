# Plot a wkpool object

Draws segments coloured by feature membership.

## Usage

``` r
# S3 method for class 'wkpool'
plot(x, col = NULL, ...)
```

## Arguments

- x:

  A wkpool object.

- col:

  Colour(s) for segments. If `NULL` (default), segments are coloured by
  feature using a built-in palette.

- ...:

  Further arguments passed to
  [`plot.default()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `x`.

## Examples

``` r
x <- wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
pool <- establish_topology(x)
plot(pool)

```
