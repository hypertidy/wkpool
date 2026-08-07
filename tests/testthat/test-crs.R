# CRS and geodesic round trip

two_squares_crs <- function(crs = "EPSG:4326") {
  wk::as_wkb(wk::wkt(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ), crs = crs))
}

test_that("establish_topology captures crs from input", {
  pool <- establish_topology(two_squares_crs())
  expect_identical(wk::wk_crs(pool), "EPSG:4326")

  pool_none <- establish_topology(wk::wkt("LINESTRING (0 0, 1 1)"))
  expect_null(wk::wk_crs(pool_none))
})

test_that("establish_topology captures geodesic flag", {
  x <- wk::wkt("LINESTRING (0 0, 10 10)", crs = "EPSG:4326", geodesic = TRUE)
  pool <- establish_topology(x)
  expect_true(wk::wk_is_geodesic(pool))

  x_planar <- wk::wkt("LINESTRING (0 0, 10 10)")
  expect_false(wk::wk_is_geodesic(establish_topology(x_planar)))
})

test_that("crs accessors and setters work", {
  pool <- establish_topology(two_squares_crs())
  pool2 <- wk::wk_set_crs(pool, "EPSG:3031")
  expect_identical(wk::wk_crs(pool2), "EPSG:3031")
  # original untouched
  expect_identical(wk::wk_crs(pool), "EPSG:4326")

  pool3 <- wk::wk_set_geodesic(pool, TRUE)
  expect_true(wk::wk_is_geodesic(pool3))
  expect_false(wk::wk_is_geodesic(pool))
})

test_that("crs survives subset and vec_restore", {
  pool <- establish_topology(two_squares_crs())
  sub <- pool[1:3]
  expect_identical(wk::wk_crs(sub), "EPSG:4326")

  x <- wk::wkt("LINESTRING (0 0, 10 10)", crs = "EPSG:4326", geodesic = TRUE)
  poolg <- establish_topology(x)
  expect_true(wk::wk_is_geodesic(poolg[1]))
})

test_that("crs survives merge_coincident, pool_compact, as_arcs", {
  pool <- establish_topology(two_squares_crs())
  merged <- merge_coincident(pool)
  expect_identical(wk::wk_crs(merged), "EPSG:4326")

  compact <- pool_compact(merged)
  expect_identical(wk::wk_crs(compact), "EPSG:4326")

  arcs <- as_arcs(merged)
  expect_identical(wk::wk_crs(arcs), "EPSG:4326")
  arcs2 <- as_arcs(merged, arc_id = FALSE)
  expect_identical(wk::wk_crs(arcs2), "EPSG:4326")

  internal <- find_internal_boundaries(merged)
  expect_identical(wk::wk_crs(internal), "EPSG:4326")
})

test_that("pool_combine resolves crs with wk rules", {
  a <- establish_topology(two_squares_crs())
  b <- establish_topology(two_squares_crs())
  expect_identical(wk::wk_crs(pool_combine(a, b)), "EPSG:4326")

  # inherit gives way to concrete
  empty <- wkpool_empty()
  expect_identical(wk::wk_crs(pool_combine(a, empty)), "EPSG:4326")

  # unequal concrete crs is an error
  d <- establish_topology(two_squares_crs("EPSG:3031"))
  expect_error(pool_combine(a, d), "not equal")
})

test_that("emitters restore crs onto wkt output", {
  pool <- merge_coincident(establish_topology(two_squares_crs()))

  expect_identical(wk::wk_crs(segments_to_wkt(pool)), "EPSG:4326")
  expect_identical(wk::wk_crs(segments_to_wkt(pool, "linestring")), "EPSG:4326")
  expect_identical(wk::wk_crs(segments_to_wkt(pool, "point")), "EPSG:4326")
  expect_identical(wk::wk_crs(arcs_to_wkt(pool)), "EPSG:4326")
  expect_identical(wk::wk_crs(cycles_to_wkt(pool, feature = FALSE)), "EPSG:4326")
})

test_that("emitters restore crs onto wkb output", {
  pool <- merge_coincident(establish_topology(two_squares_crs()))

  expect_identical(wk::wk_crs(segments_to_wkb(pool)), "EPSG:4326")
  expect_identical(wk::wk_crs(arcs_to_wkb(pool)), "EPSG:4326")
  expect_identical(wk::wk_crs(cycles_to_wkb(pool, feature = FALSE)), "EPSG:4326")
})

test_that("emitters restore geodesic onto output", {
  x <- wk::wkt(
    c("LINESTRING (0 0, 10 10)", "LINESTRING (10 10, 20 0)"),
    crs = "EPSG:4326", geodesic = TRUE
  )
  pool <- merge_coincident(establish_topology(x))
  expect_true(wk::wk_is_geodesic(segments_to_wkt(pool)))
  expect_true(wk::wk_is_geodesic(arcs_to_wkt(pool)))
  expect_true(wk::wk_is_geodesic(arcs_to_wkb(pool)))
})

test_that("empty emitters still carry crs", {
  pool <- establish_topology(two_squares_crs())
  none <- pool[0]
  expect_identical(wk::wk_crs(segments_to_wkt(none)), "EPSG:4326")
  expect_identical(wk::wk_crs(arcs_to_wkt(none)), "EPSG:4326")
  expect_identical(wk::wk_crs(arcs_to_wkb(none)), "EPSG:4326")
  expect_identical(wk::wk_crs(segments_to_wkb(none)), "EPSG:4326")
})

test_that("full round trip preserves crs", {
  x <- two_squares_crs()
  pool <- merge_coincident(establish_topology(x))
  out <- cycles_to_wkb(pool, feature = FALSE)
  expect_identical(wk::wk_crs(out), wk::wk_crs(x))
})

test_that("exported new_wkpool carries crs and geodesic", {
  v <- data.frame(.vx = 1:3, x = c(0, 1, 1), y = c(0, 0, 1))
  p <- new_wkpool(v, vx0 = c(1L, 2L), vx1 = c(2L, 3L),
                  crs = "EPSG:4326", geodesic = TRUE)
  expect_s3_class(p, "wkpool")
  expect_identical(wk::wk_crs(p), "EPSG:4326")
  expect_true(wk::wk_is_geodesic(p))

  expect_error(
    new_wkpool(v, vx0 = 1L, vx1 = 4L),
    "vx1"
  )
})

test_that("wkpool_empty defaults to crs inherit", {
  e <- wkpool_empty()
  expect_s3_class(wk::wk_crs(e), "wk_crs_inherit")
  expect_identical(wk::wk_is_geodesic(e), NA)
})

test_that("print header shows crs", {
  pool <- establish_topology(two_squares_crs())
  expect_output(print(pool), "CRS=EPSG:4326")
})
