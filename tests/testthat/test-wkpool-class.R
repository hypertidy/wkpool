# test-wkpool-class.R
# Coverage for R/wkpool.R: wkpool_empty, format/print, vctrs boilerplate,
# vec_c.wkpool, plot.wkpool, wkpool() user constructor

# wkpool() user constructor ------------------------------------------------

test_that("wkpool() constructs from vertices and segment data.frame", {
  v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  s <- data.frame(.vx0 = c(1L, 2L, 3L, 4L), .vx1 = c(2L, 3L, 4L, 1L))
  pool <- wkpool(v, s)

  expect_s3_class(pool, "wkpool")
  expect_equal(length(pool), 4)
  expect_equal(nrow(pool_vertices(pool)), 4)
})

test_that("wkpool() picks up .feature from segment data.frame", {
  v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  s <- data.frame(
    .vx0 = c(1L, 2L, 3L, 4L),
    .vx1 = c(2L, 3L, 4L, 1L),
    .feature = c(1L, 1L, 1L, 1L)
  )
  pool <- wkpool(v, s)

  expect_false(is.null(pool_feature(pool)))
  expect_equal(pool_feature(pool), c(1L, 1L, 1L, 1L))
})

test_that("wkpool() without .feature gives NULL feature", {
  v <- data.frame(.vx = 1:3, x = c(0, 1, 0.5), y = c(0, 0, 1))
  s <- data.frame(.vx0 = c(1L, 2L, 3L), .vx1 = c(2L, 3L, 1L))
  pool <- wkpool(v, s)

  expect_null(pool_feature(pool))
})

# wkpool_empty() -----------------------------------------------------------

test_that("wkpool_empty() returns zero-length wkpool", {
  e <- wkpool_empty()

  expect_s3_class(e, "wkpool")
  expect_equal(length(e), 0)
  expect_equal(nrow(pool_vertices(e)), 0)
  expect_equal(nrow(pool_segments(e)), 0)
})

test_that("wkpool_empty() has correct column structure", {
  e <- wkpool_empty()
  v <- pool_vertices(e)
  s <- pool_segments(e)

  expect_true(all(c(".vx", "x", "y") %in% names(v)))
  expect_true(all(c(".vx0", ".vx1") %in% names(s)))
})

# format / print -----------------------------------------------------------

test_that("format.wkpool shows segment arrows", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  fmt <- format(pool)

  expect_type(fmt, "character")
  expect_equal(length(fmt), length(pool))
  expect_true(all(grepl("->", fmt, fixed = TRUE)))
})

test_that("format.wkpool includes segment indices", {
  v <- data.frame(.vx = 1:3, x = c(0, 1, 0.5), y = c(0, 0, 1))
  pool <- new_wkpool(v, vx0 = c(1L, 2L), vx1 = c(2L, 3L))
  fmt <- format(pool)

  expect_true(grepl("1", fmt[1]))
  expect_true(grepl("2", fmt[1]))
})

test_that("obj_print_header.wkpool shows segment and vertex counts", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  out <- capture.output(vctrs::obj_print_header(pool))

  expect_true(any(grepl("4 segments", out)))
  expect_true(any(grepl("5 vertices", out)))
})

test_that("vec_ptype_abbr.wkpool returns wkpl", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  expect_equal(vctrs::vec_ptype_abbr(pool), "wkpl")
})

# vctrs ptype2 / cast / restore -------------------------------------------

test_that("vec_ptype2 returns empty wkpool", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  ptype <- vctrs::vec_ptype2(pool, pool)

  expect_s3_class(ptype, "wkpool")
  expect_equal(length(ptype), 0)
})

test_that("vec_cast is identity for wkpool", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  cast <- vctrs::vec_cast(pool, wkpool_empty())

  expect_s3_class(cast, "wkpool")
  expect_equal(length(cast), length(pool))
})

test_that("vec_restore preserves full pool on subset", {
  pool <- establish_topology(wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  )))
  sub <- pool[1:2]

  # Full vertex pool retained after subset
  expect_equal(nrow(pool_vertices(sub)), nrow(pool_vertices(pool)))
  expect_equal(length(sub), 2)
})

test_that("vec_restore preserves feature on subset", {
  pool <- establish_topology(wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  )))
  feat1_segs <- which(pool_feature(pool) == 1L)
  sub <- pool[feat1_segs]

  expect_false(is.null(pool_feature(sub)))
  expect_true(all(pool_feature(sub) == 1L))
})

# vec_c.wkpool ------------------------------------------------------------

test_that("vec_c errors", {
  pool_a <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  pool_b <- establish_topology(wk::as_wkb("POLYGON ((2 2, 3 2, 3 3, 2 3, 2 2))"))
  expect_error(vctrs::vec_c(pool_a, pool_b))

})

test_that("pool_combine is the supported way to combine pools", {
  pool_a <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  pool_b <- establish_topology(wk::as_wkb("POLYGON ((5 5, 6 5, 6 6, 5 6, 5 5))"))
  combined <- pool_combine(pool_a, pool_b)

  expect_s3_class(combined, "wkpool")
  expect_equal(length(combined), length(pool_a) + length(pool_b))

  v <- pool_vertices(combined)
  s <- pool_segments(combined)
  expect_true(all(s$.vx0 %in% v$.vx))
  expect_true(all(s$.vx1 %in% v$.vx))
})
# pool_segments accessor ---------------------------------------------------

test_that("pool_segments includes .feature when present", {
  pool <- establish_topology(wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  )))
  s <- pool_segments(pool)

  expect_true(".feature" %in% names(s))
  expect_true(all(s$.feature %in% c(1L, 2L)))
})

test_that("pool_segments omits .feature when absent", {
  v <- data.frame(.vx = 1:3, x = c(0, 1, 0.5), y = c(0, 0, 1))
  pool <- new_wkpool(v, vx0 = c(1L, 2L, 3L), vx1 = c(2L, 3L, 1L))
  s <- pool_segments(pool)

  expect_false(".feature" %in% names(s))
})

# new_wkpool validation ----------------------------------------------------

test_that("new_wkpool rejects invalid vertex references", {
  v <- data.frame(.vx = 1:3, x = c(0, 1, 0.5), y = c(0, 0, 1))
  expect_error(new_wkpool(v, vx0 = c(1L, 2L, 99L), vx1 = c(2L, 3L, 1L)))
})

test_that("new_wkpool rejects non-data.frame vertices", {
  expect_error(new_wkpool("not a df", vx0 = 1L, vx1 = 2L))
})

test_that("new_wkpool rejects vertices without .vx column", {
  v <- data.frame(x = 1, y = 2)
  expect_error(new_wkpool(v, vx0 = 1L, vx1 = 1L))
})

test_that("new_wkpool rejects mismatched feature length", {
  v <- data.frame(.vx = 1:3, x = c(0, 1, 0.5), y = c(0, 0, 1))
  expect_error(
    new_wkpool(v, vx0 = c(1L, 2L), vx1 = c(2L, 3L), feature = 1L)
  )
})

# plot.wkpool --------------------------------------------------------------

test_that("plot.wkpool runs without error", {
  pool <- establish_topology(wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  )))
  expect_no_error(plot(pool))
})

test_that("plot.wkpool returns x invisibly", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  result <- plot(pool)
  expect_identical(result, pool)
})

test_that("plot.wkpool accepts custom col", {
  pool <- establish_topology(wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
  expect_no_error(plot(pool, col = "red"))
})

test_that("plot.wkpool works without feature info", {
  v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  pool <- new_wkpool(v, vx0 = c(1L, 2L, 3L, 4L), vx1 = c(2L, 3L, 4L, 1L))
  expect_no_error(plot(pool))
})
