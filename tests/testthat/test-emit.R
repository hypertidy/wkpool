# Native emit: precision, z, and structure guarantees

test_that("emitters preserve coordinates at full double precision", {
  # values that do not survive decimal text formatting
  xs <- c(1 / 3, sqrt(2), 2 / 3, 1 / 3)
  ys <- c(exp(1), pi, log(2), exp(1))
  wkt_in <- sprintf(
    "POLYGON ((%.17g %.17g, %.17g %.17g, %.17g %.17g, %.17g %.17g))",
    xs[1], ys[1], xs[2], ys[2], xs[3], ys[3], xs[4], ys[4]
  )
  pool <- merge_coincident(establish_topology(wk::wkt(wkt_in)))

  # wkb path: bitwise identical
  cc <- wk::wk_coords(cycles_to_wkb(pool, feature = FALSE))
  expect_true(all(cc$x %in% xs))
  expect_true(all(cc$y %in% ys))

  cc_arc <- wk::wk_coords(arcs_to_wkb(pool))
  expect_true(all(cc_arc$x %in% xs))

  cc_seg <- wk::wk_coords(segments_to_wkb(pool, "linestring"))
  expect_true(all(cc_seg$x %in% xs))

  # wkt path: text is inherently formatted (wk's writer, 16 significant
  # digits) - the guarantee is that our wkt adds no loss beyond wk's own
  # standard behaviour, i.e. it is exactly as_wkt() of the wkb path
  expect_identical(
    cycles_to_wkt(pool, feature = FALSE),
    wk::as_wkt(cycles_to_wkb(pool, feature = FALSE))
  )
})

test_that("z is carried through the emitters", {
  x <- wk::as_wkb(wk::wkt("LINESTRING Z (0 0 1, 1 1 2, 2 0 3)"))
  pool <- establish_topology(x)

  expect_true(all(wk::wk_meta(segments_to_wkb(pool, "linestring"))$has_z))
  expect_true(all(wk::wk_meta(segments_to_wkb(pool, "point"))$has_z))
  expect_true(all(wk::wk_meta(segments_to_wkb(pool, "multilinestring"))$has_z))
  expect_true(all(wk::wk_meta(arcs_to_wkb(pool))$has_z))

  cc <- wk::wk_coords(arcs_to_wkb(merge_coincident(pool)))
  expect_identical(cc$z, c(1, 2, 3))
})

test_that("wkt emitters are exact derivations of the wkb emitters", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ), crs = "EPSG:4326")
  pool <- merge_coincident(establish_topology(x))

  expect_identical(
    wk::as_wkb(arcs_to_wkt(pool)),
    arcs_to_wkb(pool)
  )
  expect_identical(
    wk::as_wkb(segments_to_wkt(pool)),
    segments_to_wkb(pool)
  )
  expect_identical(
    wk::as_wkb(cycles_to_wkt(pool, feature = FALSE)),
    cycles_to_wkb(pool, feature = FALSE)
  )
})

test_that("multilinestring type still collects all segments into one feature", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  out <- segments_to_wkb(pool, "multilinestring")
  expect_length(out, 1L)
  expect_identical(wk::wk_meta(out)$geometry_type, 5L)
})

test_that("multiple outers per feature still produce a MULTIPOLYGON", {
  x <- wk::as_wkb(paste0(
    "MULTIPOLYGON (((0 0, 0 1, 1 1, 1 0, 0 0)), ",
    "((2 0, 2 1, 3 1, 3 0, 2 0)))"
  ))
  pool <- merge_coincident(establish_topology(x))
  # rings above traverse clockwise (negative shoelace area), so the
  # "sf" convention classifies both as outers
  out <- cycles_to_wkb(pool, feature = TRUE, convention = "sf")
  expect_length(out, 1L)
  expect_identical(wk::wk_meta(out)$geometry_type, 6L)
})

test_that("rings are closed by construction", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- merge_coincident(establish_topology(x))
  out <- cycles_to_wkt(pool, feature = FALSE)
  cc <- wk::wk_coords(out)
  # first and last ring coordinate identical
  expect_identical(cc$x[1], cc$x[nrow(cc)])
  expect_identical(cc$y[1], cc$y[nrow(cc)])
})

test_that("emitters still carry crs and geodesic (native path)", {
  g <- wk::wkt(
    c("LINESTRING (0 0, 10 10)", "LINESTRING (10 10, 20 0)"),
    crs = "EPSG:4326", geodesic = TRUE
  )
  pool <- merge_coincident(establish_topology(g))

  out <- arcs_to_wkb(pool)
  expect_identical(wk::wk_crs(out), "EPSG:4326")
  expect_true(wk::wk_is_geodesic(out))

  out2 <- segments_to_wkb(pool, "multilinestring")
  expect_identical(wk::wk_crs(out2), "EPSG:4326")
})
