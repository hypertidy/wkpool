# test-merge.R
# merge_coincident reduces vertices, preserves structure

two_squares <- function() {
  wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
}

test_that("merge_coincident reduces duplicate vertices", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  v_before <- pool_vertices(pool)
  v_after <- pool_vertices(merged)

  # Two squares share 2 vertices (1,0) and (1,1)
  expect_lt(nrow(v_after), nrow(v_before))
  # 10 raw coords -> 6 unique
  expect_equal(nrow(v_after), 6L)
})

test_that("merge_coincident preserves segment count", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  # Same number of segments before and after
  expect_equal(length(merged), length(pool))
})

test_that("merge_coincident preserves feature info", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)

  feat_before <- pool_feature(pool)
  feat_after <- pool_feature(merged)

  expect_false(is.null(feat_after))
  expect_equal(length(feat_after), length(feat_before))
  expect_equal(sort(unique(feat_after)), c(1L, 2L))
})

test_that("merge_coincident with tolerance works", {
  # Two squares with very slightly offset shared edge
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1.0001 0, 2 0, 2 1, 1.0001 1, 1.0001 0))"
  ))

  pool <- establish_topology(x)

  # Exact merge: no reduction at shared edge
  exact <- merge_coincident(pool, tolerance = 0)
  v_exact <- pool_vertices(exact)

  # Tolerance merge: reduces
  tol <- merge_coincident(pool, tolerance = 0.001)
  v_tol <- pool_vertices(tol)

  expect_lt(nrow(v_tol), nrow(v_exact))
})

test_that("merge_coincident on already-clean pool is idempotent", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  merged2 <- merge_coincident(merged)

  expect_equal(nrow(pool_vertices(merged)), nrow(pool_vertices(merged2)))
  expect_equal(length(merged), length(merged2))
})

test_that("merge_coincident produces contiguous .vx ids", {
  pool <- establish_topology(two_squares())
  merged <- merge_coincident(pool)
  v <- pool_vertices(merged)

  expect_equal(v$.vx, seq_len(nrow(v)))
})
