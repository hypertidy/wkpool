test_that("tri_neighbours() finds the triangle across each edge", {
  v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  tri <- new_wkpool_tri(v, vx0 = c(1L, 1L), vx1 = c(2L, 3L), vx2 = c(3L, 4L))
  nb <- tri_neighbours(tri)
  expect_equal(dim(nb), c(2L, 3L))
  ## edge 1-3 is edge 3 of triangle 1 and edge 1 of triangle 2
  expect_equal(nb[1, ], c(NA, NA, 2L))
  expect_equal(nb[2, ], c(1L, NA, NA))

  ## symmetric on a larger mesh, and every inner edge has a neighbour
  nb <- tri_neighbours(grid_tri(4L))
  ok <- !is.na(nb)
  i <- row(nb)[ok]
  j <- nb[ok]
  expect_true(all(vapply(seq_along(i), function(k) i[k] %in% nb[j[k], ], logical(1))))
  ## 4 x 4 cells: 16 hull edges of 96
  expect_equal(sum(!ok), 16L)
})

test_that("an edge used by three triangles has no neighbour", {
  v <- data.frame(.vx = 1:5, x = c(0, 1, 0.5, 0.5, 0.2), y = c(0, 0, 1, -1, 2))
  tri <- new_wkpool_tri(v, vx0 = c(1L, 2L, 1L), vx1 = c(2L, 1L, 2L),
                        vx2 = c(3L, 4L, 5L))
  nb <- tri_neighbours(tri)
  expect_true(all(is.na(nb[, 1])))
})

test_that("tri_measures() matches the polygons on a coverage", {
  skip_if_not_installed("geos")
  tri <- jitter_tri(6L)
  g <- random_groups(6L, 5L)
  m <- tri_measures(tri, g)
  geom <- group_geos(tri, g)
  expect_equal(m$group, 1:5)
  expect_equal(m$area, geos::geos_area(geom))
  expect_equal(m$perimeter, geos::geos_length(geom))
  ctr <- geos::geos_centroid(geom)
  expect_equal(m$centroid_x, geos::geos_x(ctr))
  expect_equal(m$centroid_y, geos::geos_y(ctr))
  expect_equal(m$xmin, geos::geos_xmin(geom))
  expect_equal(m$ymin, geos::geos_ymin(geom))
  expect_equal(m$xmax, geos::geos_xmax(geom))
  expect_equal(m$ymax, geos::geos_ymax(geom))
  ## the point is inside its group
  pt <- geos::geos_make_point(m$point_x, m$point_y)
  expect_true(all(geos::geos_contains(geom, pt)))
})

test_that("tri_measures() handles overlapping and empty groups", {
  tri <- grid_tri(3L)
  ## group 1 is the left column, 2 the bottom row, 4 everything, 3 is empty
  cells_left <- c(1, 4, 7)
  cells_bottom <- 1:3
  g <- data.frame(
    triangle = c(cells_left, cells_left + 9, cells_bottom, cells_bottom + 9, 1:18),
    group = c(rep(1, 6), rep(2, 6), rep(4, 18))
  )
  m <- tri_measures(tri, g)
  expect_equal(m$area, c(3, 3, 0, 9))
  expect_equal(m$perimeter, c(8, 8, 0, 12))
  expect_equal(m$centroid_x, c(0.5, 1.5, NA, 1.5))
  expect_equal(m$centroid_y, c(1.5, 0.5, NA, 1.5))
  expect_true(is.na(m$xmin[3]))
  ## duplicated pairs count once
  m2 <- tri_measures(tri, rbind(g, g))
  expect_equal(m2, m)
})

test_that("tri_measures() takes the feature as the default group", {
  m <- tri_measures(grid_tri(2L))
  expect_equal(nrow(m), 4L)
  expect_equal(m$area, rep(1, 4))
  expect_equal(m$perimeter, rep(4, 4))
})

test_that("tri_relate() matches geos_relate() on a coverage", {
  skip_if_not_installed("geos")
  tri <- jitter_tri(7L, seed = 3L)
  g <- random_groups(7L, 6L, seed = 4L)
  geom <- group_geos(tri, g)
  pairs <- expand.grid(a = 1:6, b = 1:6)
  r <- tri_relate(tri, g, pairs)
  expect_equal(r$relate, geos::geos_relate(geom[r$a], geom[r$b]))
  ## the default lists exactly the pairs that are not disjoint
  d <- tri_relate(tri, g)
  expect_true(all(d$a < d$b))
  nd <- r[r$a < r$b & r$relate != "FF2FF1212", ]
  expect_equal(sort(paste(d$a, d$b)), sort(paste(nd$a, nd$b)))
})

test_that("tri_relate() matches geos_relate() on overlapping groups", {
  skip_if_not_installed("geos")
  tri <- jitter_tri(6L, seed = 5L)
  nt <- length(tri)
  set.seed(6)
  k <- 8L
  g <- do.call(rbind, lapply(seq_len(k), function(i) {
    ## a random block of cells
    i0 <- sample(0:4, 1)
    j0 <- sample(0:4, 1)
    w <- sample(1:3, 1)
    h <- sample(1:3, 1)
    cell <- expand.grid(i = i0 + seq_len(w) - 1L, j = j0 + seq_len(h) - 1L)
    cell <- cell[cell$i < 6 & cell$j < 6, ]
    id <- cell$j * 6L + cell$i + 1L
    data.frame(triangle = c(id, id + 36L), group = i)
  }))
  polys <- lapply(seq_len(k), function(i) {
    idx <- g$triangle[g$group == i]
    unclass(tri_dissolve(tri[idx], rep(1L, length(idx))))
  })
  geom <- geos::as_geos_geometry(wk::wkb(unlist(polys, recursive = FALSE)))
  pairs <- expand.grid(a = seq_len(k), b = seq_len(k))
  r <- tri_relate(tri, g, pairs)
  expect_equal(r$relate, geos::geos_relate(geom[r$a], geom[r$b]))
})

test_that("tri_relate() reads simple cases", {
  ## two unit squares side by side and their union
  v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
  tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
                        vx2 = c(3, 4, 6, 3))
  g <- data.frame(triangle = c(1, 2, 3, 4, 1, 2, 3, 4),
                  group = c(1, 1, 2, 2, 3, 3, 3, 3))
  r <- tri_relate(tri, g, rbind(c(1, 2), c(1, 3), c(3, 1), c(1, 1)))
  expect_equal(r$relate, c("FF2F11212", "2FF11F212", "212F11FF2", "2FFF1FFF2"))
  expect_error(tri_relate(tri, g, cbind(1, 5)), "group ids")
})

test_that("tri_boundary() gives the boundary of each group", {
  skip_if_not_installed("geos")
  tri <- jitter_tri(6L, seed = 7L)
  g <- random_groups(6L, 4L, seed = 8L)
  b <- tri_boundary(tri, g)
  expect_s3_class(b, "wkpool")
  len <- tapply(geos::geos_length(geos::as_geos_geometry(b)), pool_feature(b), sum)
  geom <- group_geos(tri, g)
  expect_equal(unname(as.numeric(len)), geos::geos_length(geom))
  ## directed with the group on the left: the edges sum to the area
  s <- pool_segments(b)
  v <- pool_vertices(b)
  i0 <- vx_match(s$.vx0, v$.vx)
  i1 <- vx_match(s$.vx1, v$.vx)
  a <- tapply((v$x[i0] * v$y[i1] - v$x[i1] * v$y[i0]) / 2, pool_feature(b), sum)
  expect_equal(unname(as.numeric(a)), geos::geos_area(geom))
})

test_that("tri_boundary(once = TRUE) keeps a shared edge once", {
  tri <- grid_tri(2L)
  expect_length(tri_boundary(tri), 16L)
  once <- tri_boundary(tri, once = TRUE)
  expect_length(once, 12L)
  s <- pool_segments(once)
  key <- paste(pmin(s$.vx0, s$.vx1), pmax(s$.vx0, s$.vx1))
  expect_false(anyDuplicated(key) > 0)
})

test_that("tri_clip_rect() matches geos_clip_by_rect()", {
  skip_if_not_installed("geos")
  tri <- jitter_tri(6L, seed = 9L)
  rect <- c(0.7, 1.3, 4.6, 5.2)
  out <- tri_clip_rect(tri, rect)
  expect_s3_class(out, "wkpool_tri")
  expect_equal(length(attr(out, "source")), length(out))
  f <- pool_feature(tri)
  geom <- group_geos(tri, f)
  want <- geos::geos_area(geos::geos_clip_by_rect(geom, wk::rct(rect[1], rect[2], rect[3], rect[4])))
  got <- tri_measures(out, pool_feature(out))
  expect_equal(got$area, want[seq_len(nrow(got))])
  ## every output triangle is counter-clockwise and inside the rectangle
  t <- pool_triangles(out)
  v <- pool_vertices(out)
  i0 <- vx_match(t$.vx0, v$.vx)
  i1 <- vx_match(t$.vx1, v$.vx)
  i2 <- vx_match(t$.vx2, v$.vx)
  cr <- (v$x[i1] - v$x[i0]) * (v$y[i2] - v$y[i0]) -
    (v$x[i2] - v$x[i0]) * (v$y[i1] - v$y[i0])
  expect_true(all(cr > 0))
  used <- unique(c(i0, i1, i2))
  expect_true(all(v$x[used] >= rect[1] & v$x[used] <= rect[3]))
  expect_true(all(v$y[used] >= rect[2] & v$y[used] <= rect[4]))
})

test_that("tri_clip_rect() keeps the clipped pieces a mesh", {
  tri <- jitter_tri(5L, seed = 10L)
  out <- tri_clip_rect(tri, wk::rct(0.5, 0.5, 3.5, 2.5))
  ## one group over everything: one outer ring, the rectangle, no holes
  m <- tri_measures(out, rep(1L, length(out)))
  expect_equal(m$area, 3 * 2)
  expect_equal(m$perimeter, 2 * (3 + 2))
  nb <- tri_neighbours(out)
  ## the hull edges are exactly the rectangle's sides
  expect_equal(m$xmin, 0.5)
  expect_equal(m$ymax, 2.5)
  b <- tri_boundary(out, rep(1L, length(out)))
  expect_equal(length(b), sum(is.na(nb)))
})

test_that("tri_clip_rect() drops what is outside and keeps what is inside", {
  tri <- grid_tri(3L)
  expect_length(tri_clip_rect(tri, c(10, 10, 11, 11)), 0L)
  same <- tri_clip_rect(tri, c(-1, -1, 4, 4))
  expect_length(same, length(tri))
  expect_equal(attr(same, "source"), seq_along(tri))
  expect_error(tri_clip_rect(tri, c(1, 1, 0, 0)), "xmin")
})

test_that("tri_convex_hull() matches geos_convex_hull()", {
  skip_if_not_installed("geos")
  tri <- jitter_tri(6L, seed = 11L)
  g <- random_groups(6L, 4L, seed = 12L)
  h <- tri_convex_hull(tri, g)
  expect_s3_class(h, "wk_wkb")
  expect_length(h, 4L)
  geom <- group_geos(tri, g)
  want <- geos::geos_convex_hull(geom)
  expect_equal(geos::geos_area(h), geos::geos_area(want))
  expect_true(all(geos::geos_equals(h, want)))
})

test_that("tri_convex_hull() gives POLYGON EMPTY for an empty group", {
  tri <- grid_tri(2L)
  h <- tri_convex_hull(tri, c(1, NA, 3, NA, 1, NA, 3, NA))
  expect_equal(wk::as_wkt(h)[2], wk::wkt("POLYGON EMPTY"))
})
