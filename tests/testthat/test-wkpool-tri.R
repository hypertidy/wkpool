tri_signed_area <- function(tri) {
  v <- pool_vertices(tri)
  t <- pool_triangles(tri)
  i0 <- match(t$.vx0, v$.vx)
  i1 <- match(t$.vx1, v$.vx)
  i2 <- match(t$.vx2, v$.vx)
  ((v$x[i1] - v$x[i0]) * (v$y[i2] - v$y[i0]) -
     (v$x[i2] - v$x[i0]) * (v$y[i1] - v$y[i0])) / 2
}

polys <- wk::wkb(wk::as_wkb(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.3, 0.2 0.5, 0.4 0.5, 0.2 0.3))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))",
  "MULTIPOLYGON (((3 0, 4 0, 4 1, 3 0)), ((5 0, 6 0, 6 1, 5 0)))",
  "LINESTRING (0 0, 5 5)"
)), crs = "EPSG:3857")

test_that("pool_triangulate reuses the vertex pool unchanged", {
  skip_if_not_installed("decido")
  pool <- establish_topology(polys)
  tri <- pool_triangulate(pool)
  expect_s3_class(tri, "wkpool_tri")
  expect_identical(pool_vertices(tri), pool_vertices(pool))
  expect_length(tri, 11L)
  t <- pool_triangles(tri)
  expect_named(t, c(".vx0", ".vx1", ".vx2", ".feature", ".part"))
  expect_equal(t$.feature, c(rep(1L, 7), 2L, 2L, 3L, 3L))
  expect_equal(length(unique(t$.part[t$.feature == 3L])), 2L)
  ## ring closing vertices are never used
  closing <- c(5L, 9L, 14L, 18L, 22L)
  expect_false(any(unlist(t[1:3]) %in% closing))
})

test_that("triangles are counter-clockwise and cover the polygon area", {
  skip_if_not_installed("decido")
  tri <- pool_triangulate(establish_topology(polys))
  a <- tri_signed_area(tri)
  expect_true(all(a > 0))
  f <- pool_triangles(tri)$.feature
  expect_equal(as.vector(tapply(a, f, sum)), c(1 - 0.02, 1, 1))
})

test_that("merging later gives the same triangles as merging first", {
  skip_if_not_installed("decido")
  pool <- establish_topology(polys)
  late <- merge_coincident(pool_triangulate(pool))
  early <- pool_triangulate(merge_coincident(pool))
  expect_s3_class(late, "wkpool_tri")
  expect_identical(pool_triangles(late), pool_triangles(early))
  expect_identical(pool_vertices(late), pool_vertices(early))
})

test_that("tri_segments gives boundary and internal edges", {
  skip_if_not_installed("decido")
  tri <- merge_coincident(pool_triangulate(establish_topology(polys)))
  s <- tri_segments(tri)
  expect_s3_class(s, "wkpool")
  expect_length(s, 3L * length(tri))
  seg <- pool_segments(s)
  key <- paste(pmin(seg$.vx0, seg$.vx1), pmax(seg$.vx0, seg$.vx1))
  uses <- table(key)
  ## every edge is used once (boundary) or twice (internal)
  expect_true(all(uses %in% c(1L, 2L)))
  ## boundary edges: 4 + 3 outer/hole on feature 1, 4 on feature 2,
  ## 3 + 3 on feature 3, less the one edge features 1 and 2 share
  expect_equal(sum(uses == 1L), 4 + 3 + 4 + 3 + 3 - 2)
})

test_that("wk_handle presents one POLYGON per triangle with crs", {
  skip_if_not_installed("decido")
  tri <- pool_triangulate(establish_topology(polys))
  out <- wk::as_wkb(tri)
  expect_length(out, length(tri))
  expect_identical(wk::wk_crs(out), "EPSG:3857")
  expect_true(all(wk::wk_meta(out)$geometry_type == 3L))
  expect_identical(wk::wk_crs(tri), "EPSG:3857")
})

test_that("subsetting keeps the pool and attributes", {
  skip_if_not_installed("decido")
  tri <- pool_triangulate(establish_topology(polys))
  sub <- tri[2:3]
  expect_s3_class(sub, "wkpool_tri")
  expect_identical(pool_vertices(sub), pool_vertices(tri))
  expect_identical(wk::wk_crs(sub), "EPSG:3857")
  expect_identical(pool_triangles(sub), pool_triangles(tri)[2:3, ], ignore_attr = TRUE)
})

test_that("no polygons gives an empty triangle pool", {
  skip_if_not_installed("decido")
  tri <- pool_triangulate(establish_topology(wk::wkt("LINESTRING (0 0, 1 1)")))
  expect_s3_class(tri, "wkpool_tri")
  expect_length(tri, 0L)
  expect_length(wk::as_wkb(tri), 0L)
})

test_that("Z is carried to the handled geometry", {
  skip_if_not_installed("decido")
  x <- wk::wkt("POLYGON Z ((0 0 1, 1 0 2, 0 1 3, 0 0 1))")
  tri <- pool_triangulate(establish_topology(x))
  expect_true(wk::wk_meta(wk::as_wkb(tri))$has_z)
})

test_that("new_wkpool_tri validates vertex ids", {
  v <- data.frame(.vx = 1:3, x = c(0, 1, 0), y = c(0, 0, 1))
  expect_error(new_wkpool_tri(v, 1L, 2L, 4L))
  expect_s3_class(new_wkpool_tri(v, 1L, 2L, 3L), "wkpool_tri")
  expect_error(pool_triangles(establish_topology(wk::wkt("LINESTRING (0 0, 1 1)"))),
               "wkpool_tri")
})
