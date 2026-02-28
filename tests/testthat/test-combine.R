# test-combine.R
# pool_combine, subsetting, pool_compact, wkpool constructor

# pool_combine -------------------------------------------------------------

test_that("pool_combine merges two pools", {
  pool_a <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  pool_b <- establish_topology(wk::as_wkb("POLYGON ((5 5, 6 5, 6 6, 5 6, 5 5))"))
  combined <- pool_combine(pool_a, pool_b)

  expect_s3_class(combined, "wkpool")

  # Combined should have vertices from both
  v <- pool_vertices(combined)
  expect_equal(nrow(v), nrow(pool_vertices(pool_a)) + nrow(pool_vertices(pool_b)))

  # Combined should have segments from both
  expect_equal(length(combined), length(pool_a) + length(pool_b))
})

test_that("pool_combine remaps vertex indices", {
  pool_a <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  pool_b <- establish_topology(wk::as_wkb("POLYGON ((5 5, 6 5, 6 6, 5 6, 5 5))"))
  combined <- pool_combine(pool_a, pool_b)

  v <- pool_vertices(combined)
  s <- pool_segments(combined)

  # All segment refs should be valid vertex IDs
  all_vx <- v$.vx
  expect_true(all(s$.vx0 %in% all_vx))
  expect_true(all(s$.vx1 %in% all_vx))
})

test_that("pool_combine handles single pool", {
  pool_a <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  combined <- pool_combine(pool_a)

  expect_s3_class(combined, "wkpool")
  expect_equal(length(combined), length(pool_a))
})

test_that("pool_combine handles empty", {
  combined <- pool_combine()
  expect_s3_class(combined, "wkpool")
  expect_equal(length(combined), 0)
})

test_that("pool_combine preserves features", {
  pool_a <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  pool_b <- establish_topology(wk::as_wkb("POLYGON ((5 5, 6 5, 6 6, 5 6, 5 5))"))
  combined <- pool_combine(pool_a, pool_b)

  feat <- pool_feature(combined)
  expect_false(is.null(feat))
})

# Subsetting ---------------------------------------------------------------

test_that("[ subsetting returns wkpool", {
  pool <- establish_topology(wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  )))
  sub <- pool[1:3]

  expect_s3_class(sub, "wkpool")
  expect_equal(length(sub), 3)
})

test_that("[ subsetting retains full vertex pool", {
  pool <- establish_topology(wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  )))
  sub <- pool[1:2]

  # Full pool is retained even though only 2 segments selected
  v_full <- pool_vertices(pool)
  v_sub <- pool_vertices(sub)
  expect_equal(nrow(v_sub), nrow(v_full))
})

# pool_compact -------------------------------------------------------------

test_that("pool_compact removes unreferenced vertices", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)

  # Subset to first feature's segments
  feat <- pool_feature(merged)
  sub <- merged[feat == 1L]

  # Before compact: full pool retained
  v_before <- pool_vertices(sub)

  # After compact: only referenced vertices

  compact <- pool_compact(sub)
  v_after <- pool_vertices(compact)

  expect_lte(nrow(v_after), nrow(v_before))
})

test_that("pool_compact produces valid pool", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  compact <- pool_compact(pool)

  v <- pool_vertices(compact)
  s <- pool_segments(compact)

  # All segment refs are valid
  expect_true(all(s$.vx0 %in% v$.vx))
  expect_true(all(s$.vx1 %in% v$.vx))

  # .vx is contiguous
  expect_equal(v$.vx, seq_len(nrow(v)))
})

# wkpool constructor -------------------------------------------------------

test_that("new_wkpool() constructs from vertices and segments", {
  v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  pool <- new_wkpool(v, vx0 = c(1L, 2L, 3L, 4L), vx1 = c(2L, 3L, 4L, 1L))

  expect_s3_class(pool, "wkpool")
  expect_equal(length(pool), 4)
  expect_equal(nrow(pool_vertices(pool)), 4)
})

test_that("new_wkpool() accepts feature argument", {
  v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  pool <- new_wkpool(v,
    vx0 = c(1L, 2L, 3L, 4L),
    vx1 = c(2L, 3L, 4L, 1L),
    feature = c(1L, 1L, 1L, 1L)
  )
  feat <- pool_feature(pool)

  expect_false(is.null(feat))
  expect_equal(unique(feat), 1L)
})

# Format/print -------------------------------------------------------------

test_that("format.wkpool produces character vector", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  fmt <- format(pool)

  expect_type(fmt, "character")
  expect_equal(length(fmt), length(pool))
})

# as_pslg / as_decido -----------------------------------------------------

test_that("as_pslg returns list with P and S", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  pslg <- as_pslg(pool)

  expect_type(pslg, "list")
  expect_true(all(c("P", "S") %in% names(pslg)))
  expect_true(is.matrix(pslg$P))
  expect_true(is.matrix(pslg$S))
  expect_equal(ncol(pslg$P), 2)
  expect_equal(ncol(pslg$S), 2)
})

test_that("as_decido returns list with xy and 0-indexed segments", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  dec <- as_decido(pool)

  expect_type(dec, "list")
  expect_true(all(c("xy", "segments") %in% names(dec)))

  # 0-indexed: minimum should be 0
  expect_equal(min(dec$segments), 0)
})
