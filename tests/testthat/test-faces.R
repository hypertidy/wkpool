faces_of <- function(wkt) {
  find_faces(merge_coincident(establish_topology(wk::as_wkb(wkt))))
}
face_areas <- function(f) {
  a <- unique(f[c(".face", ".face_area")])
  stats::setNames(a$.face_area, a$.face)
}

test_that("a square has one bounded face and one outer face", {
  f <- faces_of("LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)")
  expect_equal(nrow(f), 8L) # two half-edges per edge
  expect_equal(sort(unname(face_areas(f))), c(-1, 1))
  expect_true(all(f$.adjacent != f$.face))
  expect_false(any(f$.bridge))
})

test_that("bounded faces are counter-clockwise and walk closes", {
  f <- faces_of("LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)")
  inner <- f[f$.face_area > 0, ]
  # consecutive half-edges chain and the walk closes
  expect_equal(inner$.vx1, c(inner$.vx0[-1], inner$.vx0[1]))
})

test_that("a square with a cross gives four blocks and Euler holds", {
  f <- faces_of(c(
    "LINESTRING (0 0, 1 0, 2 0, 2 1, 2 2, 1 2, 0 2, 0 1, 0 0)",
    "LINESTRING (1 0, 1 1, 1 2)",
    "LINESTRING (0 1, 1 1, 2 1)"
  ))
  a <- face_areas(f)
  expect_equal(sum(a > 0), 4L)
  expect_equal(unname(a[a > 0]), rep(1, 4))
  expect_equal(sum(a), 0) # outer face cancels the bounded faces
  n_e <- nrow(f) / 2
  n_v <- length(unique(c(f$.vx0, f$.vx1)))
  expect_equal(n_v - n_e + length(a), 2) # connected planar graph
  # adjacency between blocks: each block touches two others
  blocks <- as.integer(names(a)[a > 0])
  adj <- unique(f[f$.face %in% blocks & f$.adjacent %in% blocks, c(".face", ".adjacent")])
  expect_equal(as.vector(table(adj$.face)), rep(2L, 4))
})

test_that("shared boundaries count once and self-loops are dropped", {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
  pool <- merge_coincident(establish_topology(x))
  f <- find_faces(pool)
  expect_equal(nrow(f), 2L * 7L) # 8 segments, shared edge once
  a <- face_areas(f)
  expect_equal(sort(unname(a)), c(-2, 1, 1))
  # .segment points back into the input segments
  segs <- pool_segments(pool)
  s <- segs[f$.segment, ]
  expect_true(all((s$.vx0 == f$.vx0 & s$.vx1 == f$.vx1) | (s$.vx0 == f$.vx1 & s$.vx1 == f$.vx0)))

  loop <- new_wkpool(
    vertices = data.frame(.vx = 1:3, x = c(0, 1, 0), y = c(0, 0, 1)),
    vx0 = c(1L, 2L, 3L, 2L), vx1 = c(2L, 3L, 1L, 2L)
  )
  expect_equal(nrow(find_faces(loop)), 6L)
})

test_that("a dead end lies inside its face as a bridge", {
  f <- faces_of(c(
    "LINESTRING (0 0, 2 0, 2 2, 0 2, 0 0)",
    "LINESTRING (0 0, 1 1)"
  ))
  a <- face_areas(f)
  expect_equal(sort(unname(a)), c(-4, 4)) # the dead end adds no area
  br <- f[f$.bridge, ]
  expect_equal(nrow(br), 2L)
  expect_true(all(br$.face_area > 0)) # inside the bounded face
})

test_that("two squares joined by one edge: the join is a bridge of the outer face", {
  f <- faces_of(c(
    "LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)",
    "LINESTRING (2 0, 3 0, 3 1, 2 1, 2 0)",
    "LINESTRING (1 0, 2 0)"
  ))
  a <- face_areas(f)
  expect_equal(sort(unname(a)), c(-2, 1, 1))
  br <- f[f$.bridge, ]
  expect_equal(nrow(br), 2L)
  expect_true(all(br$.face_area < 0))
})

test_that("separate components each get an outer face", {
  f <- faces_of(c(
    "LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)",
    "LINESTRING (5 5, 6 5, 6 6, 5 6, 5 5)"
  ))
  a <- face_areas(f)
  expect_equal(sort(unname(a)), c(-1, -1, 1, 1))
})

test_that("a tree has one face of zero area", {
  f <- faces_of("LINESTRING (0 0, 1 0, 2 1)")
  expect_equal(unname(face_areas(f)), 0)
  expect_true(all(f$.bridge))
})

test_that("empty and degenerate input", {
  expect_equal(nrow(find_faces(establish_topology(wk::as_wkb("POINT (0 0)")))), 0L)
  expect_named(find_faces(wkpool_empty()),
    c(".face", ".vx0", ".vx1", ".segment", ".adjacent", ".face_area", ".bridge"))
})

test_that("projected coordinates far from the origin keep exact areas", {
  f <- faces_of("LINESTRING (500000 5250000, 500100 5250000, 500100 5250100, 500000 5250100, 500000 5250000)")
  expect_equal(sort(unname(face_areas(f))), c(-10000, 10000))
})

test_that("compiled kernel matches the R reference", {
  set.seed(1)
  # a jittered grid with some edges removed: many faces, dead ends,
  # several components
  g <- expand.grid(i = 0:7, j = 0:7)
  g$x <- g$i + stats::runif(nrow(g), -0.3, 0.3)
  g$y <- g$j + stats::runif(nrow(g), -0.3, 0.3)
  id <- function(i, j) i + 8L * j + 1L
  e <- rbind(
    expand.grid(i = 0:6, j = 0:7, d = "h", stringsAsFactors = FALSE),
    expand.grid(i = 0:7, j = 0:6, d = "v", stringsAsFactors = FALSE)
  )
  e <- e[stats::runif(nrow(e)) > 0.25, ]
  a <- id(e$i, e$j)
  b <- ifelse(e$d == "h", id(e$i + 1L, e$j), id(e$i, e$j + 1L))
  pool <- new_wkpool(
    vertices = data.frame(.vx = seq_len(nrow(g)), x = g$x, y = g$y),
    vx0 = as.integer(a), vx1 = as.integer(b)
  )
  expect_identical(find_faces(pool), wkpool:::find_faces_walk_r(pool))
  f <- find_faces(pool)
  expect_equal(sum(face_areas(f)), 0, tolerance = 1e-9)
  expect_equal(nrow(f), 2L * length(pool))
})
