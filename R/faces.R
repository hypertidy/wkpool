# Face enumeration
#
# The faces of a segment graph: the regions it encloses, such as street
# blocks between roads. Each undirected edge is two directed half-edges;
# at every vertex the outgoing half-edges are ordered counter-clockwise
# by angle, and the half-edge after one arriving at a vertex is the one
# clockwise-adjacent to its twin there. Walking that rule keeps the face
# on the left and uses every half-edge exactly once, so all faces come
# out in one pass with no start choice, hashing or deduplication.

#' Find the faces of a segment graph
#'
#' Enumerate the faces of the graph formed by a pool's segments: the
#' regions it encloses (street blocks between roads, parcels between
#' boundaries), plus one outer face for each connected component.
#'
#' Segments are taken as undirected edges. A boundary carried by more than
#' one feature (the same pair of vertex ids) counts once, keeping the first
#' segment, and segments from a vertex to itself are dropped. Run
#' [merge_coincident()] first so that segments meeting at the same
#' coordinates share a vertex: faces are traced through shared vertex ids
#' only, so lines that cross without a shared vertex (a bridge over a
#' road) do not split each other, and their faces can overlap.
#'
#' Each edge gives two half-edges, one per direction. At each vertex the
#' outgoing half-edges are ordered counter-clockwise by angle, and a walk
#' always takes the next edge clockwise from the one it arrived on, which
#' keeps the face on its left. Every half-edge lies on exactly one face, so
#' the result has two rows per edge. Bounded faces are counter-clockwise
#' (positive area); the outer face of each connected component is
#' clockwise (negative area), or zero for a component that encloses
#' nothing (a tree).
#'
#' An edge with the same face on both sides (`.bridge`) is a dead end, or
#' a bridge joining two parts of the graph; its two half-edges both appear
#' in that face's walk. Drop those rows to keep only edges that separate
#' two faces.
#'
#' Only x and y are used.
#'
#' @param x A wkpool (ideally after [merge_coincident()]).
#' @return A data frame with one row per half-edge, in walk order face by
#'   face:
#'   - `.face`: face id, 1-based, in order of the lowest half-edge on it.
#'   - `.vx0`, `.vx1`: the half-edge's start and end vertex ids.
#'   - `.segment`: index of the input segment (in `x`) it comes from.
#'   - `.adjacent`: the face on the other side of the edge.
#'   - `.face_area`: signed area of the face (positive for bounded faces).
#'   - `.bridge`: `TRUE` when `.adjacent` is the same face.
#'
#' @seealso [find_neighbours()] for adjacency between input features,
#'   [find_cycles()] for the rings of polygon input.
#' @examples
#' ## a square with a cross inside: four blocks
#' x <- wk::as_wkb(c(
#'   "LINESTRING (0 0, 1 0, 2 0, 2 1, 2 2, 1 2, 0 2, 0 1, 0 0)",
#'   "LINESTRING (1 0, 1 1, 1 2)",
#'   "LINESTRING (0 1, 1 1, 2 1)"
#' ))
#' pool <- merge_coincident(establish_topology(x))
#' f <- find_faces(pool)
#' ## bounded faces and their areas
#' unique(f[f$.face_area > 0, c(".face", ".face_area")])
#' ## which blocks share an edge
#' unique(f[f$.face_area > 0 & f$.face != f$.adjacent, c(".face", ".adjacent")])
#' @export
find_faces <- function(x) {
  check_wkpool(x)
  prep <- face_input(x)
  if (is.null(prep)) return(faces_empty())
  # compiled kernel: same algorithm and ordering as the R reference below
  # (find_faces_walk_r)
  res <- find_faces_cpp(prep$i0 - 1L, prep$i1 - 1L, prep$x, prep$y)
  face_table(prep, res$order, res$face)
}

# Dense, deduplicated edge input shared by the kernel and the reference.
face_input <- function(x) {
  segs <- pool_segments(x)
  vx0 <- segs$.vx0
  vx1 <- segs$.vx1
  seg <- seq_along(vx0)
  gid <- vctrs::vec_group_id(data.frame(lo = pmin(vx0, vx1), hi = pmax(vx0, vx1)))
  keep <- vx0 != vx1 & !duplicated(gid)
  if (!any(keep)) return(NULL)
  vx0 <- vx0[keep]
  vx1 <- vx1[keep]
  seg <- seg[keep]

  ids <- sort(unique(c(vx0, vx1)))
  pool <- pool_vertices(x)
  idx <- vx_match(ids, pool$.vx)
  # centre coordinates: angles are unchanged and areas keep precision
  # for projected coordinates far from the origin
  px <- pool$x[idx]
  py <- pool$y[idx]
  list(
    i0 = match(vx0, ids), i1 = match(vx1, ids), ids = ids, seg = seg,
    x = px - mean(range(px)), y = py - mean(range(py))
  )
}

# Assemble the result from walk order and per-half-edge face (both 0-based).
face_table <- function(prep, order, face) {
  h <- order
  e <- h %/% 2L + 1L
  fwd <- h %% 2L == 0L
  a <- ifelse(fwd, prep$i0[e], prep$i1[e])
  b <- ifelse(fwd, prep$i1[e], prep$i0[e])
  f <- face[h + 1L] + 1L
  adj <- face[bitwXor(h, 1L) + 1L] + 1L
  cross <- prep$x[a] * prep$y[b] - prep$x[b] * prep$y[a]
  area <- as.vector(rowsum(cross, f, reorder = TRUE)) / 2
  data.frame(
    .face = f,
    .vx0 = prep$ids[a],
    .vx1 = prep$ids[b],
    .segment = prep$seg[e],
    .adjacent = adj,
    .face_area = area[f],
    .bridge = adj == f
  )
}

faces_empty <- function() {
  data.frame(
    .face = integer(), .vx0 = integer(), .vx1 = integer(),
    .segment = integer(), .adjacent = integer(), .face_area = numeric(),
    .bridge = logical()
  )
}

# Pure-R reference implementation of the face walk. Retained as the
# executable specification for the compiled kernel: the conformance
# tests assert identical output on shared fixtures.
find_faces_walk_r <- function(x) {
  prep <- face_input(x)
  if (is.null(prep)) return(faces_empty())
  n <- length(prep$i0)
  nh <- 2L * n
  nv <- length(prep$ids)
  h <- seq_len(nh) - 1L
  e <- h %/% 2L + 1L
  odd <- h %% 2L == 1L
  from <- ifelse(odd, prep$i1[e], prep$i0[e])
  to <- ifelse(odd, prep$i0[e], prep$i1[e])
  ang <- atan2(prep$y[to] - prep$y[from], prep$x[to] - prep$x[from])

  # rotation: outgoing half-edges by vertex, counter-clockwise, ties by id
  o <- order(from, ang, h)
  rot <- h[o]
  deg <- tabulate(from, nv)
  offset <- c(0L, cumsum(deg))
  pos <- integer(nh)
  pos[rot + 1L] <- seq_len(nh) - 1L - offset[from[o]]

  # next half-edge for every half-edge at once
  twin <- bitwXor(h, 1L)
  d <- deg[to]
  nxt <- rot[offset[to] + (pos[twin + 1L] - 1L + d) %% d + 1L]

  order_out <- integer(nh)
  face <- integer(nh)
  used <- logical(nh)
  k <- 0L
  f <- 0L
  for (start in h) {
    if (used[start + 1L]) next
    cur <- start
    repeat {
      used[cur + 1L] <- TRUE
      face[cur + 1L] <- f
      k <- k + 1L
      order_out[k] <- cur
      cur <- nxt[cur + 1L]
      if (cur == start) break
    }
    f <- f + 1L
  }
  face_table(prep, order_out, face)
}
