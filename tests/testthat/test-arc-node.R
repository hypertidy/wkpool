# test-arc-node.R
# vertex_degree, find_nodes, find_arcs, arc_node_summary, as_arcs, arcs_to_wkt

two_squares <- function() {
  x <- wk::as_wkb(c(
    "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
    "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
  ))
  pool <- establish_topology(x)
  merge_coincident(pool)
}

# vertex_degree ------------------------------------------------------------

test_that("vertex_degree returns named integer vector", {
  merged <- two_squares()
  deg <- vertex_degree(merged)

  expect_type(deg, "integer")
  expect_false(is.null(names(deg)))
})

test_that("vertex_degree correct for two adjacent squares", {
  merged <- two_squares()
  deg <- vertex_degree(merged)

  # 6 unique vertices after merge
  expect_equal(length(deg), 6)

  # Shared vertices (1,0) and (1,1) have degree 4 (two segments from each polygon)
  # Corner vertices have degree 2
  expect_true(any(deg == 4))
  expect_true(any(deg == 2))
})

# find_nodes ---------------------------------------------------------------

test_that("find_nodes returns integer vector", {
  merged <- two_squares()
  nodes <- find_nodes(merged)

  expect_type(nodes, "integer")
})

test_that("find_nodes finds degree != 2 vertices", {
  merged <- two_squares()
  nodes <- find_nodes(merged)
  deg <- vertex_degree(merged)

  # Nodes are vertices with degree != 2
  non2 <- as.integer(names(deg)[deg != 2])
  expect_setequal(nodes, non2)
})

test_that("find_nodes empty for simple closed ring", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  nodes <- find_nodes(merged)

  # All degree-2 in a simple ring => no nodes
  expect_equal(length(nodes), 0)
})

# find_arcs ----------------------------------------------------------------

test_that("find_arcs returns list of integer vectors", {
  merged <- two_squares()
  arcs <- find_arcs(merged)

  expect_type(arcs, "list")
  expect_true(all(vapply(arcs, is.integer, logical(1)) |
                    vapply(arcs, is.numeric, logical(1))))
})

test_that("find_arcs on simple ring returns one closed arc", {
  x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  pool <- establish_topology(x)
  merged <- merge_coincident(pool)
  arcs <- find_arcs(merged)

  # Single closed ring with all degree-2: one arc

  expect_equal(length(arcs), 1)

  # Arc should be closed (first == last)
  arc <- arcs[[1]]
  expect_equal(arc[1], arc[length(arc)])
})

test_that("find_arcs segments sum equals total segments", {
  merged <- two_squares()
  arcs <- find_arcs(merged)

  # Each arc has (length - 1) segments; total should equal total segments
  arc_seg_count <- sum(lengths(arcs) - 1)
  expect_equal(arc_seg_count, length(merged))
})

# as_arcs ------------------------------------------------------------------

test_that("as_arcs returns wkpool", {
  merged <- two_squares()
  arc_pool <- as_arcs(merged)

  expect_s3_class(arc_pool, "wkpool")
})

# arc_node_summary ---------------------------------------------------------

test_that("arc_node_summary returns expected structure", {
  merged <- two_squares()
  summary <- arc_node_summary(merged)

  expect_type(summary, "list")
  expected_names <- c(
    "n_vertices", "n_nodes", "n_arcs",
    "degree_distribution", "arc_length_distribution", "mean_arc_length"
  )
  expect_true(all(expected_names %in% names(summary)))
})

test_that("arc_node_summary values are consistent", {
  merged <- two_squares()
  summary <- arc_node_summary(merged)

  expect_equal(summary$n_vertices, 6)
  expect_gt(summary$n_nodes, 0)
  expect_gt(summary$n_arcs, 0)
  expect_gt(summary$mean_arc_length, 0)
})
