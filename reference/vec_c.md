# Combine many wkpool vectors into one vector

This is non-functional, wkpool does not currently support `vec_c()`.

## Usage

``` r
# S3 method for class 'wkpool'
vec_c(..., .ptype = NULL)
```

## Arguments

- ...:

  Vectors to coerce.

- .ptype:

  If `NULL`, the default, the output type is determined by computing the
  common type across all elements of `...`.

  Alternatively, you can supply `.ptype` to give the output known type.
  If `getOption("vctrs.no_guessing")` is `TRUE` you must supply this
  value: this is a convenient way to make production code demand fixed
  types.

## Value

nothing, used for a message side-effect (see Details)

## Details

Attempts to combine wkpool vectors with vec_c will suggest using
\[pool_combine()' instead.

## See also

[`pool_combine()`](https://hypertidy.github.io/wkpool/reference/pool_combine.md)
