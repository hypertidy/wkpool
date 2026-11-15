test_that("tri_locate() finds the triangle holding each point", {
  v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
  tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                        vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
  pts <- wk::xy(c(0.75, 0.25, 1.75, 1.25, 3, -1), c(0.25, 0.75, 0.5, 0.75, 0.5, 0))
  expect_identical(tri_locate(tri, pts), c(1L, 2L, 3L, 4L, NA, NA))
  ## the same from a matrix, a data frame and a list
  m <- cbind(c(0.75, 0.25), c(0.25, 0.75))
  expect_identical(tri_locate(tri, m), c(1L, 2L))
  expect_identical(tri_locate(tri, data.frame(x = m[, 1], y = m[, 2])), c(1L, 2L))
  expect_identical(tri_locate(tri, list(x = m[, 1], y = m[, 2])), c(1L, 2L))
  expect_identical(tri_locate(tri, wk::wkt(c("POINT (0.75 0.25)", "POINT (0.25 0.75)"))),
                   c(1L, 2L))
  ## missing coordinates and no points
  expect_identical(tri_locate(tri, cbind(NA_real_, 0.5)), NA_integer_)
  expect_identical(tri_locate(tri, wk::xy()), integer())
  expect_error(tri_locate(tri, cbind(1, 2, 3)), "two columns")
})

test_that("points on edges and vertices go to the lowest triangle", {
  v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
  tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                        vx2 = c(3, 4, 6, 3))
  ## the diagonal of the first square (triangles 1 and 2), the shared side
  ## of the squares (2 is not on it: triangles 1 and 4), a vertex of all
  ## four triangles, and an outer corner of triangle 2 alone
  pts <- cbind(c(0.5, 1, 1, 0), c(0.5, 0.5, 1, 1))
  expect_identical(tri_locate(tri, pts), c(1L, 1L, 1L, 2L))
  ## the outer boundary is inside
  expect_identical(tri_locate(tri, cbind(c(0, 2, 1.5), c(0.5, 0.5, 0))), c(2L, 3L, 3L))
})

test_that("tri_locate() decides near-collinear points exactly", {
  ## a long thin pair of triangles either side of the diagonal from (0, 0)
  ## to (1e8 + 1, 1e8): points just off it in floating point are on the
  ## side their exact coordinates say
  big <- 1e8
  v <- data.frame(.vx = 1:4, x = c(0, big + 1, big + 1, 0), y = c(0, 0, big, big))
  tri <- new_wkpool_tri(v, vx0 = c(1, 1), vx1 = c(2, 3), vx2 = c(3, 4))
  ## exactly on the diagonal: x = y * (big + 1) / big, at y = big / 2
  on <- cbind((big + 1) / 2, big / 2)
  expect_identical(tri_locate(tri, on), 1L)
  ## the next doubles either side of it
  x <- (big + 1) / 2
  below <- cbind(x + x * .Machine$double.eps, big / 2)
  above <- cbind(x - x * .Machine$double.eps, big / 2)
  expect_identical(tri_locate(tri, below), 1L)
  expect_identical(tri_locate(tri, above), 2L)
})

test_that("tri_locate() handles clockwise and degenerate triangles", {
  v <- data.frame(.vx = 1:5, x = c(0, 1, 1, 0, 2), y = c(0, 0, 1, 1, 0))
  ## triangle 1 is clockwise, triangle 2 has zero area
  tri <- new_wkpool_tri(v, vx0 = c(1L, 1L), vx1 = c(3L, 2L), vx2 = c(2L, 5L))
  expect_identical(tri_locate(tri, cbind(c(0.75, 1.5), c(0.25, 0))), c(1L, NA))
})

test_that("tri_locate() matches geos point in polygon on a jittered mesh", {
  skip_if_not_installed("geos")
  tri <- jitter_tri(8L)
  g <- random_groups(8L, 5L)
  set.seed(3)
  px <- stats::runif(2000, -0.5, 8.5)
  py <- stats::runif(2000, -0.5, 8.5)
  ## and every vertex, on boundaries and corners
  v <- pool_vertices(tri)
  px <- c(px, v$x)
  py <- c(py, v$y)
  i <- tri_locate(tri, cbind(px, py))
  geom <- group_geos(tri, g)
  pts <- geos::geos_make_point(px, py)
  ## the group each point is in by the mesh, and the groups geos says
  ## intersect it (a point on a shared boundary is in both)
  mesh_group <- g[i]
  for (k in seq_along(geom)) {
    hit <- geos::geos_intersects(pts, geom[k])
    ## every point the mesh puts in group k, geos does too
    expect_true(all(hit[!is.na(mesh_group) & mesh_group == k]))
    ## every point strictly inside group k (not on its boundary) the mesh
    ## puts in group k
    inside <- geos::geos_contains(geom[k], pts)
    expect_true(all(mesh_group[inside] == k, na.rm = FALSE))
  }
  ## points outside the mesh
  hull <- geos::geos_make_polygon(c(0, 8, 8, 0), c(0, 0, 8, 8))
  expect_identical(is.na(i), !geos::geos_intersects(pts, hull))
})
