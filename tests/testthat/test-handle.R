# wk_handle: a wkpool is wk-handleable

handle_squares <- function(crs = "EPSG:4326") {
  wk::as_wkb(wk::wkt(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ), crs = crs))
}

test_that("a wkpool is handleable", {
  pool <- establish_topology(handle_squares())
  expect_true(wk::is_handleable(pool))
})

test_that("wk_handle presents one linestring per segment", {
  pool <- establish_topology(handle_squares())
  out <- wk::as_wkb(pool)
  expect_length(out, length(pool))
  meta <- wk::wk_meta(pool)
  expect_true(all(meta$geometry_type == 2L))

  wkt <- wk::as_wkt(pool)
  expect_match(as.character(wkt), "^LINESTRING", all = TRUE)
})

test_that("handled output carries crs and geodesic", {
  pool <- establish_topology(handle_squares())
  expect_identical(wk::wk_crs(wk::as_wkb(pool)), "EPSG:4326")
  expect_identical(wk::wk_crs(wk::as_wkt(pool)), "EPSG:4326")

  g <- wk::wkt("LINESTRING (0 0, 10 10)", crs = "EPSG:4326", geodesic = TRUE)
  poolg <- establish_topology(g)
  expect_true(wk::wk_is_geodesic(wk::as_wkt(poolg)))
})

test_that("wk_coords on a pool returns segment endpoints at full precision", {
  # coordinates that do not survive 15-significant-digit text formatting
  x0 <- 1 / 3
  x1 <- x0 + .Machine$double.eps / 4
  v <- data.frame(.vx = 1:2, x = c(x0, sqrt(2)), y = c(exp(1), pi))
  pool <- new_wkpool(v, vx0 = 1L, vx1 = 2L)

  cc <- wk::wk_coords(pool)
  expect_identical(cc$x, c(x0, sqrt(2)))
  expect_identical(cc$y, c(exp(1), pi))
})

test_that("wk_handle respects segment direction and identity", {
  pool <- merge_coincident(establish_topology(handle_squares()))
  cc <- wk::wk_coords(pool)
  v <- pool_vertices(pool)
  s <- pool_segments(pool)

  # feature_id runs over segments; each has exactly 2 coords
  expect_identical(nrow(cc), 2L * nrow(s))
  expect_identical(cc$feature_id, rep(seq_len(nrow(s)), each = 2L))

  # endpoints match the pool lookup, in order
  i0 <- match(s$.vx0, v$.vx)
  i1 <- match(s$.vx1, v$.vx)
  expect_identical(cc$x[c(TRUE, FALSE)], v$x[i0])
  expect_identical(cc$x[c(FALSE, TRUE)], v$x[i1])
})

test_that("subset pools handle as their subset", {
  pool <- establish_topology(handle_squares())
  one <- pool[2]
  out <- wk::as_wkb(one)
  expect_length(out, 1L)
  expect_identical(wk::wk_crs(out), "EPSG:4326")
})

test_that("empty pools handle to empty vectors with crs", {
  pool <- establish_topology(handle_squares())
  none <- pool[0]
  out <- wk::as_wkb(none)
  expect_length(out, 0L)
  expect_identical(wk::wk_crs(out), "EPSG:4326")
})

test_that("z pools emit XYZ", {
  x <- wk::as_wkb(wk::wkt("LINESTRING Z (0 0 1, 1 1 2, 2 0 3)"))
  pool <- establish_topology(x)
  expect_true("z" %in% names(pool_vertices(pool)))
  meta <- wk::wk_meta(pool)
  expect_true(all(wk::wk_meta(pool)$has_z))
  cc <- wk::wk_coords(pool)
  expect_identical(cc$z, c(1, 2, 2, 3))
})

test_that("wk_vector_meta works through the handler", {
  pool <- establish_topology(handle_squares())
  vm <- wk::wk_vector_meta(pool)
  # vector-level type of a wkb stream is unknown (0) until scanned;
  # per-feature meta is the authoritative check (all LINESTRING)
  expect_identical(nrow(vm), 1L)
  expect_true(vm$geometry_type %in% c(0L, 2L))
  expect_identical(unique(wk::wk_meta(pool)$geometry_type), 2L)
})

test_that("handled coordinates agree with segments_to_wkt on simple inputs", {
  pool <- merge_coincident(establish_topology(handle_squares()))
  a <- wk::wk_coords(pool)[c("x", "y")]
  b <- wk::wk_coords(segments_to_wkt(pool, type = "linestring"))[c("x", "y")]
  expect_equal(a, b)
})
