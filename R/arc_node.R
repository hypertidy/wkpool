# Arc-node topology
#
# Arcs are maximal sequences of segments passing through degree-2 vertices.
# Nodes are vertices where degree != 2 (branch points or endpoints).

#' Calculate vertex degree (number of segments touching each vertex)
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @return Named integer vector: names are .vx, values are degree
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' vertex_degree(merged)
#'
#' @export
vertex_degree <- function(x) {
  check_wkpool(x)
  vx0 <- vctrs::field(x, ".vx0")
  vx1 <- vctrs::field(x, ".vx1")
  deg <- table(c(vx0, vx1))
  out <- as.integer(deg)
  names(out) <- names(deg)
  out
}


#' Find nodes (vertices where degree != 2)
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @return Integer vector of .vx IDs that are nodes
#'
#' @details
#' Nodes are branch points (degree 3+) or endpoints (degree 1).
#' Degree-2 vertices are pass-through points within an arc.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' find_nodes(merged)
#'
#' @export
find_nodes <- function(x) {
  check_wkpool(x)
  deg <- vertex_degree(x)
  as.integer(names(deg)[deg != 2])
}


#' Find arcs (maximal segment sequences between nodes)
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @return A list of integer vectors, each containing .vx IDs forming an arc
#'
#' @details
#' Arcs are maximal paths through degree-2 vertices. They start and end
#' at nodes (degree != 2) or form closed loops through degree-2 vertices.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' find_arcs(merged)
#'
#' @export
find_arcs <- function(x) {
  check_wkpool(x)
  segs <- pool_segments(x)
  deg <- vertex_degree(x)
  nodes <- as.integer(names(deg)[deg != 2])

  n_segs <- nrow(segs)
  if (n_segs == 0) return(list())

  # Build adjacency: for each vertex, which segments touch it?
  vx0 <- segs$.vx0
  vx1 <- segs$.vx1

  # Segment lookup by vertex
  seg_by_vertex <- list()
  for (i in seq_len(n_segs)) {
    v0 <- as.character(vx0[i])
    v1 <- as.character(vx1[i])
    seg_by_vertex[[v0]] <- c(seg_by_vertex[[v0]], i)
    seg_by_vertex[[v1]] <- c(seg_by_vertex[[v1]], i)
  }

  used <- logical(n_segs)
  arcs <- list()

  # Start arcs from nodes
  for (node in nodes) {
    node_segs <- seg_by_vertex[[as.character(node)]]
    for (start_seg in node_segs) {
      if (used[start_seg]) next

      # Walk from this node
      arc_vx <- node
      current_vx <- node
      current_seg <- start_seg

      repeat {
        used[current_seg] <- TRUE

        # Move to other end of segment
        if (vx0[current_seg] == current_vx) {
          next_vx <- vx1[current_seg]
        } else {
          next_vx <- vx0[current_seg]
        }

        arc_vx <- c(arc_vx, next_vx)

        # Stop if we hit a node
        if (next_vx %in% nodes) break

        # Continue through degree-2 vertex
        next_segs <- seg_by_vertex[[as.character(next_vx)]]
        next_segs <- next_segs[!used[next_segs]]

        if (length(next_segs) == 0) break

        current_vx <- next_vx
        current_seg <- next_segs[1]
      }

      arcs <- c(arcs, list(arc_vx))
    }
  }

  # Handle closed loops (all degree-2, no nodes)
  remaining <- which(!used)
  while (length(remaining) > 0) {
    start_seg <- remaining[1]
    start_vx <- vx0[start_seg]

    arc_vx <- start_vx
    current_vx <- start_vx
    current_seg <- start_seg

    repeat {
      used[current_seg] <- TRUE

      if (vx0[current_seg] == current_vx) {
        next_vx <- vx1[current_seg]
      } else {
        next_vx <- vx0[current_seg]
      }

      arc_vx <- c(arc_vx, next_vx)

      # Closed loop?
      if (next_vx == start_vx) break

      next_segs <- seg_by_vertex[[as.character(next_vx)]]
      next_segs <- next_segs[!used[next_segs]]

      if (length(next_segs) == 0) break

      current_vx <- next_vx
      current_seg <- next_segs[1]
    }

    arcs <- c(arcs, list(arc_vx))
    remaining <- which(!used)
  }

  arcs
}


#' Convert arcs to a wkpool of arc segments
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @param arc_id Logical: add .arc column to track arc membership?
#' @return A wkpool with segments grouped by arc
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' as_arcs(merged)
#'
#' @export
as_arcs <- function(x, arc_id = TRUE) {
  check_wkpool(x)
  arcs <- find_arcs(x)
  pool <- pool_vertices(x)

  vx0 <- integer()
  vx1 <- integer()
  arc_ids <- integer()

  for (i in seq_along(arcs)) {
    arc <- arcs[[i]]
    n <- length(arc)
    if (n < 2) next

    vx0 <- c(vx0, arc[-n])
    vx1 <- c(vx1, arc[-1])
    arc_ids <- c(arc_ids, rep(i, n - 1))
  }

  if (arc_id) {
    # Construct with arc tracking
    vctrs::new_rcrd(
      list(.vx0 = vx0, .vx1 = vx1, .arc = arc_ids),
      pool = pool,
      class = "wkpool"
    )
  } else {
    new_wkpool(pool, vx0, vx1)
  }
}


#' Summarize arc-node structure
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @return List with counts and degree distribution
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' merged <- merge_coincident(pool)
#' arc_node_summary(merged)
#'
#' @export
arc_node_summary <- function(x) {
  check_wkpool(x)
  deg <- vertex_degree(x)
  arcs <- find_arcs(x)
  nodes <- find_nodes(x)

  arc_lengths <- lengths(arcs) - 1  # segments per arc

  list(
    n_vertices = length(deg),
    n_nodes = length(nodes),
    n_arcs = length(arcs),
    degree_distribution = table(deg),
    arc_length_distribution = table(arc_lengths),
    mean_arc_length = mean(arc_lengths)
  )
}
