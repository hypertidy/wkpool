# Arc-node topology
#
# Arcs are maximal sequences of segments passing through degree-2 vertices.
# Nodes are vertices where degree != 2 (branch points or endpoints).

#' Calculate vertex degree (number of segments touching each vertex)
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @param quotient Count in the quotient graph: duplicated shared
#'   boundaries (the same undirected vertex pair carried by more than
#'   one feature) count once. A vertex interior to a shared boundary
#'   has quotient degree 2 - the degree-2 invariant holds at this
#'   level - while true junctions (three or more distinct edges) stay
#'   nodes. Default FALSE counts every segment.
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
vertex_degree <- function(x, quotient = FALSE) {
  check_wkpool(x)
  vx0 <- vctrs::field(x, ".vx0")
  vx1 <- vctrs::field(x, ".vx1")
  if (quotient) {
    q <- quotient_edges(vx0, vx1)
    vx0 <- q$vx0
    vx1 <- q$vx1
  }
  ids <- sort(unique(c(vx0, vx1)))
  out <- tabulate(match(c(vx0, vx1), ids), nbins = length(ids))
  names(out) <- ids
  out
}

# Collapse segments to unique undirected edges (the quotient graph of
# a pool whose shared boundaries are duplicated per feature). The
# first occurrence keeps its direction; multiplicity is discarded
# whatever its count (2 for an ordinary shared boundary, more where
# several features stack).
quotient_edges <- function(vx0, vx1) {
  gid <- vctrs::vec_group_id(data.frame(lo = pmin(vx0, vx1), hi = pmax(vx0, vx1)))
  keep <- !duplicated(gid)
  list(vx0 = vx0[keep], vx1 = vx1[keep])
}


#' Find nodes (vertices where degree != 2)
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @inheritParams vertex_degree
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
find_nodes <- function(x, quotient = FALSE) {
  check_wkpool(x)
  deg <- vertex_degree(x, quotient = quotient)
  as.integer(names(deg)[deg != 2])
}


#' Find arcs (maximal segment sequences between nodes)
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @param quotient Walk the quotient graph: duplicated shared
#'   boundaries collapse to a single undirected edge (first occurrence
#'   keeps its direction), so a boundary shared by two features
#'   becomes ONE arc rather than one per feature, and its interior
#'   vertices are degree-2 pass-throughs. This is the TopoJSON-style
#'   arc decomposition of a polygon layer. Default FALSE walks every
#'   segment.
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
find_arcs <- function(x, quotient = FALSE) {
  check_wkpool(x)
  segs <- pool_segments(x)

  n_segs <- nrow(segs)
  if (n_segs == 0) return(list())

  vx0 <- segs$.vx0
  vx1 <- segs$.vx1

  if (quotient) {
    q <- quotient_edges(vx0, vx1)
    vx0 <- q$vx0
    vx1 <- q$vx1
  }

  # Dense vertex indexing: work in positions 1..n_vertices, keep the
  # original ids for output
  ids <- sort(unique(c(vx0, vx1)))
  i0 <- match(vx0, ids)
  i1 <- match(vx1, ids)

  # compiled kernel: same algorithm, same ordering semantics, same
  # output as the R reference below (find_arcs_walk_r); ids mapped
  # back inside the kernel
  return(find_arcs_cpp(i0 - 1L, i1 - 1L, ids))
}

# Pure-R reference implementation of the arc walk. Retained as the
# executable specification for the compiled kernel: the conformance
# tests assert identical output on shared fixtures.
find_arcs_walk_r <- function(x, quotient = FALSE) {
  segs <- pool_segments(x)

  n_segs <- nrow(segs)
  if (n_segs == 0) return(list())

  vx0 <- segs$.vx0
  vx1 <- segs$.vx1

  if (quotient) {
    q <- quotient_edges(vx0, vx1)
    vx0 <- q$vx0
    vx1 <- q$vx1
    n_segs <- length(vx0)
  }

  ids <- sort(unique(c(vx0, vx1)))
  i0 <- match(vx0, ids)
  i1 <- match(vx1, ids)

  deg <- tabulate(c(i0, i1), nbins = length(ids))
  node_pos <- which(deg != 2)
  is_node <- logical(length(ids))
  is_node[node_pos] <- TRUE

  # Adjacency: for each vertex position, the touching segments, in the
  # same order the old per-segment append produced (segment ascending,
  # start endpoint before end endpoint within a segment)
  verts <- as.vector(rbind(i0, i1))
  adj <- split(rep(seq_len(n_segs), each = 2L),
               factor(verts, levels = seq_along(ids)))
  # per-vertex cursor into adj, so each unused-segment scan is amortized
  cursor <- rep(1L, length(ids))

  next_unused <- function(v) {
    a <- adj[[v]]
    k <- cursor[v]
    while (k <= length(a) && used[a[k]]) k <- k + 1L
    cursor[v] <<- k
    if (k <= length(a)) a[k] else 0L
  }

  used <- logical(n_segs)
  arcs <- vector("list", n_segs)
  n_arcs <- 0L
  buf <- integer(n_segs + 1L)

  walk <- function(start_pos, start_seg, stop_at_node) {
    k <- 1L
    buf[k] <<- start_pos
    current <- start_pos
    seg <- start_seg

    repeat {
      used[seg] <<- TRUE
      nxt <- if (i0[seg] == current) i1[seg] else i0[seg]
      k <- k + 1L
      buf[k] <<- nxt

      if (stop_at_node && is_node[nxt]) break
      if (!stop_at_node && nxt == start_pos) break

      seg <- next_unused(nxt)
      if (seg == 0L) break
      current <- nxt
    }

    ids[buf[seq_len(k)]]
  }

  # Start arcs from nodes
  for (node in node_pos) {
    for (start_seg in adj[[node]]) {
      if (used[start_seg]) next
      n_arcs <- n_arcs + 1L
      arcs[[n_arcs]] <- walk(node, start_seg, stop_at_node = TRUE)
    }
  }

  # Handle closed loops (all degree-2, no nodes)
  for (start_seg in seq_len(n_segs)) {
    if (used[start_seg]) next
    n_arcs <- n_arcs + 1L
    arcs[[n_arcs]] <- walk(i0[start_seg], start_seg, stop_at_node = FALSE)
  }

  arcs[seq_len(n_arcs)]
}


#' Convert arcs to a wkpool of arc segments
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @param arc_id Logical: add .arc column to track arc membership?
#' @inheritParams find_arcs
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
as_arcs <- function(x, arc_id = TRUE, quotient = FALSE) {
  check_wkpool(x)
  arcs <- find_arcs(x, quotient = quotient)
  pool <- pool_vertices(x)

  keep <- lengths(arcs) >= 2
  vx0 <- unlist(lapply(arcs[keep], function(a) a[-length(a)]))
  vx1 <- unlist(lapply(arcs[keep], function(a) a[-1]))
  arc_ids <- rep(which(keep), lengths(arcs[keep]) - 1L)
  if (is.null(vx0)) vx0 <- integer()
  if (is.null(vx1)) vx1 <- integer()

  crs <- attr(x, "crs", exact = TRUE)
  geodesic <- attr(x, "geodesic", exact = TRUE)

  if (arc_id) {
    # Construct with arc tracking
    vctrs::new_rcrd(
      list(.vx0 = vx0, .vx1 = vx1, .arc = arc_ids),
      pool = pool,
      crs = crs,
      geodesic = geodesic,
      class = "wkpool"
    )
  } else {
    new_wkpool(pool, vx0, vx1, crs = crs, geodesic = geodesic)
  }
}


#' Summarize arc-node structure
#'
#' @param x A wkpool (ideally after merge_coincident)
#' @inheritParams find_arcs
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
arc_node_summary <- function(x, quotient = FALSE) {
  check_wkpool(x)
  deg <- vertex_degree(x, quotient = quotient)
  arcs <- find_arcs(x, quotient = quotient)
  nodes <- find_nodes(x, quotient = quotient)

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
