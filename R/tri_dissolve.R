# tri_dissolve: triangles to polygons, by group
#
# Moved here from polymer's fuse(), where it dissolves the overlay mesh by
# label set. The ring walk is topological: the next boundary edge at a
# vertex is found by turning through the triangle fan around it, not by
# comparing angles.

#' Dissolve triangles into polygons by group
#'
#' Merge the triangles of a triangle pool into one MULTIPOLYGON per group,
#' along shared triangle edges. With the default `group`, the triangles of
#' each feature are dissolved back into that feature's polygons.
#'
#' `group` is one integer per triangle: a dense, 1-based group id, with `NA`
#' for a triangle that belongs to no group (it is left out, and its edges
#' become boundary). This is the `set_id` of a labelled mesh, so a mesh
#' labelled by the covering input features (an overlay) dissolves into its
#' pieces with `tri_dissolve(x, set_id)`, while the table from set to
#' features is kept by the caller. Element `k` of the result is group `k`;
#' a group id with no triangles gives `MULTIPOLYGON EMPTY`.
#'
#' Triangles must be counter-clockwise and form a triangulation (no
#' overlaps), with adjacent triangles sharing vertex ids: run
#' [merge_coincident()] first on a pool whose triangles do not already
#' share vertices. Triangles are joined only across an edge whose two
#' triangles share both of its vertex ids, so triangles that touch at a
#' vertex or along part of an edge stay separate.
#'
#' Rings follow the edges of the triangles, so pieces that touch share
#' vertices exactly. Outer rings are counter-clockwise and holes clockwise.
#' A group has one polygon for each edge-connected part, and a boundary
#' that passes through a vertex twice (a hole touching its outer ring, for
#' example) is split there, so every ring is simple and rings meet only at
#' points. Only x and y are used.
#'
#' @param x A `wkpool_tri`.
#' @param group Integer group id per triangle (1-based, `NA` to leave a
#'   triangle out). The default is the triangles' feature, or one group
#'   when the pool has no feature.
#' @param ... Unused.
#'
#' @returns A [wk::wkb()] vector of MULTIPOLYGON, one per group id from 1 to
#'   the largest in `group`, with the pool's crs.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.3, 0.2 0.5, 0.4 0.5, 0.2 0.3))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' if (requireNamespace("decido", quietly = TRUE)) {
#'   tri <- merge_coincident(pool_triangulate(establish_topology(x)))
#'   ## back to the input polygons
#'   wk::as_wkt(tri_dissolve(tri))
#'   ## everything as one polygon, with the hole kept
#'   wk::as_wkt(tri_dissolve(tri, rep(1L, length(tri))))
#' }
#'
#' @export
tri_dissolve <- function(x, group = NULL, ...) {
  check_wkpool_tri(x)
  nt <- length(x)
  if (is.null(group)) {
    group <- pool_feature(x) %||% rep(1L, nt)
  }
  if (length(group) != nt) {
    stop("`group` must have one value per triangle", call. = FALSE)
  }
  if (!is.numeric(group) || any(!is.na(group) & (group < 1 | group != round(group)))) {
    stop("`group` must be positive whole numbers or NA", call. = FALSE)
  }
  group <- as.integer(group)
  crs <- attr(x, "crs", exact = TRUE)
  ng <- if (all(is.na(group))) 0L else max(group, na.rm = TRUE)
  empty <- unclass(wk::as_wkb(wk::wkt("MULTIPOLYGON EMPTY")))[[1L]]
  keep <- !is.na(group)
  if (!any(keep)) {
    return(wk::wkb(rep(list(empty), ng), crs = crs))
  }

  v <- pool_vertices(x)
  t <- pool_triangles(x)[keep, , drop = FALSE]
  group <- group[keep]
  rings <- dissolve_rings(vx_match(t$.vx0, v$.vx), vx_match(t$.vx1, v$.vx),
                          vx_match(t$.vx2, v$.vx), group, v$x, v$y)

  polys <- wk::wk_polygon(wk::xy(rings$x, rings$y),
                          feature_id = rings$poly, ring_id = rings$ring)
  pg <- rings$group[!duplicated(rings$poly)]
  out <- wk::wk_collection(polys, wk::wk_geometry_type("multipolygon"),
                           feature_id = pg)
  ## one geometry per group id, in id order
  geom <- unclass(wk::as_wkb(out))
  idx <- match(seq_len(ng), unique(pg))
  out <- geom[idx]
  out[is.na(idx)] <- list(empty)
  wk::wkb(out, crs = crs)
}

## Boundary rings of each group, in output order (by group, then polygon,
## outer ring first). `t0`, `t1`, `t2` are row indices into `vx`, `vy`.
## Returns a data frame of ring vertices (not closed) with `ring`, `poly`
## (both in 1, 2, ... output order) and `group`.
##
## Triangles are counter-clockwise, so the boundary of a group, walked along
## triangle edges whose twin is not in the group, has the group on its left:
## outer rings come out counter-clockwise and holes clockwise.
dissolve_rings <- function(t0, t1, t2, group, vx, vy) {
  nv <- length(vx)
  nt <- length(t0)

  ## directed edges, three per triangle: edge k of triangle i is row
  ## (k - 1) * nt + i, and runs from corner k to corner k + 1
  e0 <- c(t0, t1, t2)
  e1 <- c(t1, t2, t0)
  et <- rep(seq_len(nt), 3L)
  ## the next edge round the same triangle, leaving the vertex this one ends at
  after <- c(seq_len(nt) + nt, seq_len(nt) + 2L * nt, seq_len(nt))
  twin <- match((e1 - 1) * as.double(nv) + e0, (e0 - 1) * as.double(nv) + e1)
  inner <- !is.na(twin) & group[et] == group[et[twin]]

  comp <- edge_components(et[inner], et[twin[inner]], nt)

  ## the boundary edge that follows each boundary edge: turn through the fan
  ## of the group's triangles around the vertex it ends at (clockwise, from
  ## the triangle of the incoming edge) to the first edge leaving the group
  b <- which(!inner)
  nb <- length(b)
  j <- after[b]
  for (k in seq_len(nt)) {
    w <- which(inner[j])
    if (length(w) < 1L) break
    j[w] <- after[twin[j[w]]]
  }
  nxt <- match(j, b)

  ## walk the cycles
  ring <- integer(nb)
  pos <- integer(nb)
  r <- 0L
  for (s in seq_len(nb)) {
    if (ring[s] > 0L) next
    r <- r + 1L
    k <- 0L
    e <- s
    while (ring[e] == 0L) {
      k <- k + 1L
      ring[e] <- r
      pos[e] <- k
      e <- nxt[e]
    }
  }
  o <- order(ring, pos)
  bu <- e0[b]
  ring <- split_rings(ring[o], bu[o])
  o <- o[order(ring, seq_along(ring))]
  ring <- sort(ring)
  vxr <- bu[o]
  owner <- et[b][o]

  ## signed area per ring: positive is an outer ring
  xr <- vx[vxr]
  yr <- vy[vxr]
  nxt_in_ring <- unlist(lapply(split(seq_along(ring), ring),
                               function(i) c(i[-1L], i[1L])), use.names = FALSE)
  ## translate each ring to its first vertex before the shoelace sum: with
  ## absolute coordinates the rounding error (about |x| |y| 1e-16) can exceed
  ## the area of a small ring and give it the wrong sign or exactly zero
  ring_first <- match(ring, ring)
  xs <- xr - xr[ring_first]
  ys <- yr - yr[ring_first]
  area <- rowsum((xs * ys[nxt_in_ring] - xs[nxt_in_ring] * ys) / 2, ring)[, 1L]
  first <- !duplicated(ring)
  ring_comp <- comp[owner[first]]
  ring_group <- group[owner[first]]
  is_outer <- area > 0

  ## each ring belongs to a polygon (an outer ring and its holes)
  tri_x <- (vx[t0] + vx[t1] + vx[t2]) / 3
  tri_y <- (vy[t0] + vy[t1] + vy[t2]) / 3
  poly <- assign_holes(seq_along(area), ring_comp, is_outer, area,
                       owner[first], ring, xr, yr, tri_x, tri_y)

  ## rings in output order: by group, then polygon, outer ring first
  poly_group <- ring_group[poly]
  ring_rank <- match(seq_along(area), order(poly_group, poly, !is_outer))
  row_order <- order(ring_rank[ring], seq_along(ring))
  row_poly <- poly[ring[row_order]]
  data.frame(x = xr[row_order], y = yr[row_order],
             ring = ring_rank[ring[row_order]],
             poly = match(row_poly, unique(row_poly)),
             group = ring_group[ring[row_order]])
}

## Split rings that visit a vertex more than once. `ring` and `vx` are in
## walk order; the closed sub-walk between the two visits becomes its own
## ring and the rest of the ring keeps one visit. Returns new ring ids.
split_rings <- function(ring, vx) {
  key <- (as.double(ring) - 1) * (max(vx) + 1) + vx
  if (!anyDuplicated(key)) return(ring)
  next_id <- max(ring)
  for (r in unique(ring[duplicated(key)])) {
    idx <- which(ring == r)
    repeat {
      j <- which(duplicated(vx[idx]))[1L]
      if (is.na(j)) break
      i <- match(vx[idx][j], vx[idx])
      next_id <- next_id + 1L
      ring[idx[i:(j - 1L)]] <- next_id
      idx <- idx[-(i:(j - 1L))]
    }
  }
  ring
}

## Connected components of triangles joined by inner edges (label
## propagation with pointer jumping); returns a component id per triangle.
edge_components <- function(a, b, n) {
  lab <- seq_len(n)
  repeat {
    m <- pmin(lab[a], lab[b])
    o <- order(m, decreasing = TRUE)
    new <- lab
    new[a[o]] <- m[o]
    new <- pmin(new, lab)
    new <- new[new]
    if (identical(new, lab)) break
    lab <- new
  }
  match(lab, unique(lab))
}

## Polygon id for each ring. A component with one outer ring takes all its
## holes. Otherwise each hole goes to the smallest outer ring of its
## component that contains the centroid of the triangle on its inner side
## (a triangle of the group, next to the hole).
assign_holes <- function(id, comp, is_outer, area, owner, ring, xr, yr,
                         tri_x, tri_y) {
  poly <- integer(length(id))
  poly[is_outer] <- id[is_outer]
  for (h in id[!is_outer]) {
    cand <- id[is_outer & comp == comp[h]]
    if (length(cand) == 1L) {
      poly[h] <- cand
      next
    }
    px <- tri_x[owner[h]]
    py <- tri_y[owner[h]]
    inside <- vapply(cand, function(o) {
      i <- ring == o
      point_in_ring(px, py, xr[i], yr[i])
    }, logical(1))
    cand <- if (any(inside)) cand[inside] else cand
    poly[h] <- cand[which.min(abs(area[cand]))]
  }
  poly
}

## Even-odd crossing test of one point against one ring (not closed).
point_in_ring <- function(px, py, x, y) {
  j <- c(length(x), seq_len(length(x) - 1L))
  up <- (y > py) != (y[j] > py)
  xc <- x[up] + (py - y[up]) * (x[j][up] - x[up]) / (y[j][up] - y[up])
  sum(px < xc) %% 2L == 1L
}
