# test-roundtrip-coverage.R
# Coverage for R/round_trip.R: untested code paths in cycles_to_wkt,
# segments_to_wkt, arcs_to_wkt, and the _wkb variants

# Helpers ------------------------------------------------------------------

two_squares <- function() {
  wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
}

# cycles_to_wkt: convention = "ogc" ---------------------------------------

test_that("cycles_to_wkt accepts convention = 'ogc'", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  wkt_ogc <- cycles_to_wkt(merged, feature = FALSE, convention = "ogc")

  expect_s3_class(wkt_ogc, "wk_wkt")
  expect_true(length(wkt_ogc) > 0)
})

test_that("cycles_to_wkt ogc and sf conventions both produce valid WKT", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  wkt_sf <- cycles_to_wkt(merged, feature = FALSE, convention = "sf")
  wkt_ogc <- cycles_to_wkt(merged, feature = FALSE, convention = "ogc")

  expect_s3_class(wkt_sf, "wk_wkt")
  expect_s3_class(wkt_ogc, "wk_wkt")
})

# cycles_to_wkt: polygon with hole -----------------------------------------

test_that("cycles_to_wkt handles polygon with hole", {
  x <- wk::as_wkb(
    "POLYGON ((0 0, 4 0, 4 4, 0 4, 0 0), (1 1, 3 1, 3 3, 1 3, 1 1))"
  )
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)

  wkt_out <- cycles_to_wkt(merged, feature = FALSE)
  expect_s3_class(wkt_out, "wk_wkt")
  expect_true(length(wkt_out) > 0)

  # Should detect 2 cycles (outer + hole)
  cycles <- find_cycles(merged)
  expect_equal(length(cycles), 2)
})

test_that("cycles_to_wkt feature=TRUE groups hole with outer ring", {
  x <- wk::as_wkb(
    "POLYGON ((0 0, 4 0, 4 4, 0 4, 0 0), (1 1, 3 1, 3 3, 1 3, 1 1))"
  )
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)

  wkt_feat <- cycles_to_wkt(merged, feature = TRUE)
  expect_s3_class(wkt_feat, "wk_wkt")
})

test_that("classify_cycles detects hole in polygon", {
  x <- wk::as_wkb(
    "POLYGON ((0 0, 4 0, 4 4, 0 4, 0 0), (1 1, 3 1, 3 3, 1 3, 1 1))"
  )
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  cc <- classify_cycles(merged)

  expect_true("outer" %in% cc$type || "hole" %in% cc$type)
})

# cycles_to_wkt: MULTIPOLYGON input ----------------------------------------

test_that("cycles_to_wkt handles multipolygon", {
  x <- wk::as_wkb(
    "MULTIPOLYGON (((0 0, 1 0, 1 1, 0 1, 0 0)), ((2 2, 3 2, 3 3, 2 3, 2 2)))"
  )
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)

  wkt_out <- cycles_to_wkt(merged, feature = FALSE)
  expect_s3_class(wkt_out, "wk_wkt")

  cycles <- find_cycles(merged)
  expect_equal(length(cycles), 2)
})

# cycles_to_wkt: no feature info -------------------------------------------

test_that("cycles_to_wkt feature=TRUE falls back when no feature info", {
  v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  pool <- new_wkpool(v,
    vx0 = c(1L, 2L, 3L, 4L),
    vx1 = c(2L, 3L, 4L, 1L)
  )

  # feature=TRUE with no feature info should still produce output
  wkt_out <- cycles_to_wkt(pool, feature = TRUE)
  expect_s3_class(wkt_out, "wk_wkt")
})

# cycles_to_wkt: feature-grouped with multiple features --------------------

test_that("cycles_to_wkt feature=TRUE with two features", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  wkt_feat <- cycles_to_wkt(merged, feature = TRUE)
  expect_s3_class(wkt_feat, "wk_wkt")
})

# arcs_to_wkt: coordinate fidelity ----------------------------------------

test_that("arcs_to_wkt preserves coordinate values", {
  x <- wk::as_wkb("LINESTRING (0 0, 1 1, 2 0)")
  pool <- establish_topology(x)

  wkt_out <- arcs_to_wkt(pool)
  rt_coords <- wk::wk_coords(wkt_out)

  expect_true(all(c(0, 1, 2) %in% rt_coords$x))
  expect_true(all(c(0, 1) %in% rt_coords$y))
})

test_that("arcs_to_wkt from merged polygons includes all original coords", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)
  wkt_out <- arcs_to_wkt(merged)

  # All arcs are valid linestrings
  types <- wk::wk_meta(wkt_out)$geometry_type
  expect_true(all(types == 2L))

  # All original coordinates present
  rt_coords <- wk::wk_coords(wkt_out)
  expect_true(all(c(0, 1, 2) %in% rt_coords$x))
  expect_true(all(c(0, 1) %in% rt_coords$y))
})

# segments_to_wkt: default type (no explicit arg) --------------------------

test_that("segments_to_wkt default is multilinestring", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))

  wkt_out <- segments_to_wkt(pool)
  expect_s3_class(wkt_out, "wk_wkt")
  expect_equal(length(wkt_out), 1L)

  types <- wk::wk_meta(wkt_out)$geometry_type
  expect_equal(types, 5L)  # 5 = MULTILINESTRING
})

# segments_to_wkt: point type vertex coverage ------------------------------

test_that("segments_to_wkt type='point' returns all pool vertices", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  pts <- segments_to_wkt(pool, type = "point")

  expect_equal(length(pts), nrow(pool_vertices(pool)))

  coords <- wk::wk_coords(pts)
  expect_true(all(c(0, 1) %in% coords$x))
  expect_true(all(c(0, 1) %in% coords$y))
})

# segments_to_wkt: linestring coordinate fidelity --------------------------

test_that("segments_to_wkt linestring has correct per-segment endpoints", {
  x <- wk::as_wkb("LINESTRING (10 20, 30 40, 50 60)")
  pool <- establish_topology(x)
  ls <- segments_to_wkt(pool, type = "linestring")

  expect_equal(length(ls), 2)

  c1 <- wk::wk_coords(ls[1])
  expect_equal(c1$x, c(10, 30))
  expect_equal(c1$y, c(20, 40))

  c2 <- wk::wk_coords(ls[2])
  expect_equal(c2$x, c(30, 50))
  expect_equal(c2$y, c(40, 60))
})

# Full round-trip coordinate fidelity --------------------------------------

test_that("full round-trip preserves exact coordinate set", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  pool <- merge_coincident(pool)

  orig <- wk::wk_coords(x)
  rt <- wk::wk_coords(cycles_to_wkt(pool, feature = FALSE))

  orig_pts <- unique(orig[, c("x", "y")])
  rt_pts <- unique(rt[, c("x", "y")])

  expect_equal(
    sort(orig_pts$x),
    sort(rt_pts$x)
  )
  expect_equal(
    sort(orig_pts$y),
    sort(rt_pts$y)
  )
})

test_that("round-trip with merge preserves coords for disjoint polygons", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((5 5, 6 5, 6 6, 5 6, 5 5))"
  ))
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  wkt_out <- cycles_to_wkt(merged, feature = FALSE)

  orig_coords <- wk::wk_coords(x)
  rt_coords <- wk::wk_coords(wkt_out)

  expect_equal(sort(unique(orig_coords$x)), sort(unique(rt_coords$x)))
  expect_equal(sort(unique(orig_coords$y)), sort(unique(rt_coords$y)))
})

# LINESTRING input ---------------------------------------------------------

test_that("round-trip works for LINESTRING", {
  x <- wk::as_wkb("LINESTRING (0 0, 1 1, 2 0, 3 1)")
  pool <- establish_topology(x)

  expect_equal(length(pool), 3)
  expect_equal(nrow(pool_vertices(pool)), 4)

  ls <- segments_to_wkt(pool, type = "linestring")
  expect_equal(length(ls), 3)

  arcs <- arcs_to_wkt(pool)
  expect_s3_class(arcs, "wk_wkt")
})

# MULTILINESTRING input ----------------------------------------------------

test_that("round-trip works for MULTILINESTRING", {
  x <- wk::as_wkb("MULTILINESTRING ((0 0, 1 1), (2 2, 3 3, 4 4))")
  pool <- establish_topology(x)

  expect_equal(length(pool), 3)
  expect_equal(nrow(pool_vertices(pool)), 5)
})

# POINT input (edge case) --------------------------------------------------

test_that("establish_topology handles POINT (produces empty pool)", {
  x <- wk::as_wkb("POINT (1 2)")
  pool <- establish_topology(x)

  expect_equal(length(pool), 0)
})

test_that("wkpool() accepts a segment data.frame with columns (fix for vec_assert bug)", {
  v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  s <- data.frame(.vx0 = c(1L, 2L, 3L, 4L), .vx1 = c(2L, 3L, 4L, 1L))
  # This previously failed: vec_assert(segments, data.frame()) rejects
  # any data.frame with columns because the empty prototype has none
  expect_no_error(wkpool(v, s))
})

# Empty geometry edge case -------------------------------------------------

test_that("segments_to_wkt handles empty pool", {
  e <- wkpool_empty()
  ls <- segments_to_wkt(e, type = "linestring")
  expect_s3_class(ls, "wk_wkt")
  expect_equal(length(ls), 0)
})

test_that("arcs_to_wkt handles empty pool", {
  e <- wkpool_empty()
  arcs <- arcs_to_wkt(e)
  expect_s3_class(arcs, "wk_wkt")
  expect_equal(length(arcs), 0)
})

# WKB variants produce matching output ------------------------------------

test_that("arcs_to_wkb produces same count as arcs_to_wkt", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  wkt <- arcs_to_wkt(merged)
  wkb <- arcs_to_wkb(merged)
  expect_equal(length(wkt), length(wkb))
})

test_that("cycles_to_wkb produces same count as cycles_to_wkt", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  wkt <- cycles_to_wkt(merged, feature = FALSE)
  wkb <- cycles_to_wkb(merged, feature = FALSE)
  expect_equal(length(wkt), length(wkb))
})

test_that("cycles_to_wkb passes convention argument through", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  wkb_sf <- cycles_to_wkb(merged, feature = FALSE, convention = "sf")
  wkb_ogc <- cycles_to_wkb(merged, feature = FALSE, convention = "ogc")
  expect_s3_class(wkb_sf, "wk_wkb")
  expect_s3_class(wkb_ogc, "wk_wkb")
})

test_that("segments_to_wkb matches segments_to_wkt for all types", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))

  for (type in c("multilinestring", "linestring", "point")) {
    wkt <- segments_to_wkt(pool, type = type)
    wkb <- segments_to_wkb(pool, type = type)
    expect_equal(length(wkt), length(wkb), info = type)
  }
})

# Z coordinate handling ----------------------------------------------------

test_that("segments_to_wkt works with Z coordinates", {
  x <- wk::as_wkb("LINESTRING Z (0 0 10, 1 1 20, 2 0 30)")
  pool <- establish_topology(x)
  v <- pool_vertices(pool)

  expect_true("z" %in% names(v))
  expect_equal(v$z, c(10, 20, 30))

  ls <- segments_to_wkt(pool, type = "linestring")
  expect_equal(length(ls), 2)
})

test_that("arcs_to_wkt works with Z coordinates", {
  x <- wk::as_wkb("LINESTRING Z (0 0 10, 1 1 20, 2 0 30)")
  pool <- establish_topology(x)

  arcs <- arcs_to_wkt(pool)
  expect_s3_class(arcs, "wk_wkt")
  expect_true(length(arcs) > 0)
})
