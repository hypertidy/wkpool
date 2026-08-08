# Quotient graph semantics: duplicated shared boundaries count once

quotient_fixture <- function() {
  # two features sharing a two-segment boundary through (1, 0.5)
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 0.5, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0.5, 1 0))"
  ))
  merge_coincident(establish_topology(x))
}

test_that("quotient degree: boundary-interior vertices are degree 2", {
  m <- quotient_fixture()
  v <- pool_vertices(m)
  mid <- v$.vx[v$x == 1 & v$y == 0.5]
  junctions <- v$.vx[(v$x == 1 & v$y == 0) | (v$x == 1 & v$y == 1)]

  deg <- vertex_degree(m, quotient = TRUE)
  expect_identical(unname(deg[as.character(mid)]), 2L)
  # true junctions keep three distinct edges
  expect_true(all(deg[as.character(junctions)] == 3L))

  # default unchanged (guarded elsewhere, asserted here for contrast)
  expect_identical(unname(vertex_degree(m)[as.character(mid)]), 4L)
})

test_that("quotient nodes: only true junctions", {
  m <- quotient_fixture()
  v <- pool_vertices(m)
  junctions <- sort(v$.vx[(v$x == 1 & v$y == 0) | (v$x == 1 & v$y == 1)])

  expect_identical(sort(find_nodes(m, quotient = TRUE)), junctions)
  expect_identical(length(find_nodes(m)), 3L)  # default: midpoint included
})

test_that("quotient arcs: shared boundary becomes one arc", {
  m <- quotient_fixture()
  expect_identical(length(find_arcs(m)), 6L)               # per-feature walk
  arcs_q <- find_arcs(m, quotient = TRUE)
  expect_identical(length(arcs_q), 3L)                     # left, right, shared once

  # the shared arc passes through the midpoint as an interior vertex
  v <- pool_vertices(m)
  mid <- v$.vx[v$x == 1 & v$y == 0.5]
  holds_mid <- vapply(arcs_q, function(a) mid %in% a, logical(1))
  expect_identical(sum(holds_mid), 1L)
  shared <- arcs_q[[which(holds_mid)]]
  expect_identical(length(shared), 3L)  # junction, midpoint, junction
  # first occurrence keeps its direction: feature 1 traverses upward
  expect_identical(v$y[match(shared, v$.vx)], c(0, 0.5, 1))
})

test_that("kernel and R reference agree under quotient", {
  m <- quotient_fixture()
  expect_identical(find_arcs(m, quotient = TRUE),
                   wkpool:::find_arcs_walk_r(m, quotient = TRUE))
})

test_that("edges stacked more than twice still collapse to one", {
  v <- data.frame(.vx = 1:2, x = c(0, 1), y = c(0, 0))
  pool <- new_wkpool(v, vx0 = c(1L, 2L, 1L), vx1 = c(2L, 1L, 2L))
  deg <- vertex_degree(pool, quotient = TRUE)
  expect_identical(unname(deg), c(1L, 1L))
  expect_identical(length(find_arcs(pool, quotient = TRUE)), 1L)
})

test_that("quotient is a no-op on pools without duplicated edges", {
  x <- wk::as_wkb("LINESTRING (0 0, 1 0, 2 0, 3 1)")
  m <- merge_coincident(establish_topology(x))
  expect_identical(vertex_degree(m, quotient = TRUE), vertex_degree(m))
  expect_identical(find_arcs(m, quotient = TRUE), find_arcs(m))
  expect_identical(find_nodes(m, quotient = TRUE), find_nodes(m))
})

test_that("arcs_to_wkb quotient emits each shared boundary once", {
  m <- quotient_fixture()
  wkb_default <- arcs_to_wkb(m)
  wkb_q <- arcs_to_wkb(m, quotient = TRUE)
  expect_length(wkb_default, 6L)
  expect_length(wkb_q, 3L)

  # coordinates at the shared midpoint appear in exactly one quotient arc
  cc <- wk::wk_coords(wkb_q)
  expect_identical(sum(cc$x == 1 & cc$y == 0.5), 1L)

  # and wkt derivation matches
  expect_identical(wk::as_wkb(arcs_to_wkt(m, quotient = TRUE)), wkb_q)
})

test_that("as_arcs and arc_node_summary pass quotient through", {
  m <- quotient_fixture()
  a <- as_arcs(m, quotient = TRUE)
  expect_identical(max(vctrs::field(a, ".arc")), 3L)

  s <- arc_node_summary(m, quotient = TRUE)
  expect_identical(s$n_arcs, 3L)
  expect_identical(s$n_nodes, 2L)
})
