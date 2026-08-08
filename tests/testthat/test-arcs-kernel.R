# The compiled arc kernel must reproduce the R reference exactly

conformance <- function(x) {
  expect_identical(find_arcs(x), wkpool:::find_arcs_walk_r(x))
}

test_that("kernel matches reference: adjacent polygons (nodes + arcs)", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
  conformance(merge_coincident(establish_topology(x)))
})

test_that("kernel matches reference: closed loops, no nodes", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  conformance(merge_coincident(establish_topology(x)))
})

test_that("kernel matches reference: open linestrings (degree-1 endpoints)", {
  x <- wk::as_wkb(c(
    "LINESTRING (0 0, 1 0, 2 0)",
    "LINESTRING (2 0, 3 1, 4 0)",
    "LINESTRING (2 0, 2 -1)"
  ))
  conformance(merge_coincident(establish_topology(x)))
})

test_that("kernel matches reference: multipolygon with hole", {
  x <- wk::as_wkb(paste0(
    "MULTIPOLYGON (((0 0, 0 10, 10 10, 10 0, 0 0), ",
    "(2 2, 4 2, 4 4, 2 4, 2 2)), ",
    "((20 0, 20 5, 25 5, 25 0, 20 0)))"
  ))
  conformance(merge_coincident(establish_topology(x)))
})

test_that("kernel matches reference: grid with many degree-4 nodes", {
  ij <- expand.grid(i = 0:5, j = 0:5)
  g <- wk::as_wkb(wk::wkt(sprintf(
    "POLYGON ((%d %d, %d %d, %d %d, %d %d, %d %d))",
    ij$i, ij$j, ij$i + 1L, ij$j, ij$i + 1L, ij$j + 1L, ij$i, ij$j + 1L, ij$i, ij$j
  )))
  conformance(merge_coincident(establish_topology(g)))
})

test_that("kernel matches reference: subset and shuffled pools", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
  merged <- merge_coincident(establish_topology(x))
  conformance(merged[2:6])
  set.seed(42)
  conformance(merged[sample(length(merged))])
})

test_that("kernel matches reference: unmerged pool (duplicate coordinates)", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
  conformance(establish_topology(x))
})
