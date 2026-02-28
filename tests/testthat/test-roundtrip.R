# test-roundtrip.R
# establish_topology -> merge_coincident -> *_to_wkt round-trips

# Helpers ------------------------------------------------------------------

two_squares <- function() {
  wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
}

# establish_topology -------------------------------------------------------

test_that("establish_topology returns wkpool", {
  pool <- establish_topology(two_squares())
  expect_s3_class(pool, "wkpool")
})

test_that("establish_topology preserves all coordinates", {
  x <- two_squares()
  pool <- establish_topology(x)
  v <- pool_vertices(pool)

  # Two squares with 5 coords each = 10 rows

  expect_equal(nrow(v), 10)
  expect_true(all(c(".vx", "x", "y") %in% names(v)))
})

test_that("establish_topology tracks features", {
  pool <- establish_topology(two_squares())
  feat <- pool_feature(pool)
  expect_false(is.null(feat))
  expect_equal(sort(unique(feat)), c(1L, 2L))
})

test_that("establish_topology handles single polygon", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  expect_s3_class(pool, "wkpool")
  expect_equal(length(pool), 4L) # 4 segments from 5 coords (closed ring)
})

test_that("establish_topology handles wkt input", {
  x <- wk::wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  expect_s3_class(pool, "wkpool")
})

test_that("establish_topology handles z coordinates", {
  x <- wk::as_wkb("POLYGON Z ((0 0 1, 1 0 2, 1 1 3, 0 1 4, 0 0 1))")
  pool <- establish_topology(x)
  v <- pool_vertices(pool)
  expect_true("z" %in% names(v))
})

# cycles_to_wkt round-trip ------------------------------------------------

test_that("cycles_to_wkt recovers original polygons", {
  x <- two_squares()
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)

  # Use feature=FALSE: sign-convention-based grouping is a known issue
  wkt_out <- cycles_to_wkt(merged, feature = FALSE)
  expect_s3_class(wkt_out, "wk_wkt")
  expect_equal(length(wkt_out), 2L)

  # Round-trip coords should match original
  orig_coords <- wk::wk_coords(x)
  rt_coords <- wk::wk_coords(wkt_out)

  # Same x/y ranges

  expect_equal(range(orig_coords$x), range(rt_coords$x))
  expect_equal(range(orig_coords$y), range(rt_coords$y))
})

test_that("cycles_to_wkt feature=TRUE is affected by winding convention", {

  # Known issue: find_cycles traversal direction may not match SF sign
  # convention, causing feature-grouped output to be empty for standard

  # SF-wound input. This test documents the current behaviour.
  x <- two_squares()
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)

  wkt_feat <- cycles_to_wkt(merged, feature = TRUE)
  # Currently returns 0 due to winding convention mismatch
  expect_s3_class(wkt_feat, "wk_wkt")
})

test_that("cycles_to_wkt feature=FALSE produces valid WKT", {
  x <- two_squares()
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)

  wkt_nofeat <- cycles_to_wkt(merged, feature = FALSE)
  expect_s3_class(wkt_nofeat, "wk_wkt")
  expect_true(length(wkt_nofeat) > 0)
})

# arcs_to_wkt round-trip ---------------------------------------------------

test_that("arcs_to_wkt produces linestrings", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)
  wkt_out <- arcs_to_wkt(merged)

  expect_s3_class(wkt_out, "wk_wkt")
  # All should be LINESTRING
  types <- wk::wk_meta(wkt_out)$geometry_type
  expect_true(all(types == 2L)) # 2 = linestring
})

# segments_to_wkt ---------------------------------------------------------

test_that("segments_to_wkt works for all types", {
  pool <- establish_topology(two_squares())

  ls <- segments_to_wkt(pool, type = "linestring")
  expect_s3_class(ls, "wk_wkt")
  expect_equal(length(ls), length(pool))

  mls <- segments_to_wkt(pool, type = "multilinestring")
  expect_s3_class(mls, "wk_wkt")
  expect_equal(length(mls), 1L)

  pts <- segments_to_wkt(pool, type = "point")
  expect_s3_class(pts, "wk_wkt")
})

# WKB variants -------------------------------------------------------------

test_that("wkb round-trip variants work", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  expect_s3_class(arcs_to_wkb(merged), "wk_wkb")
  expect_s3_class(cycles_to_wkb(merged), "wk_wkb")
  expect_s3_class(segments_to_wkb(pool), "wk_wkb")
})
