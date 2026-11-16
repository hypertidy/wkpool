# tri_*(): geometry questions answered on a triangle pool
#
# A triangle pool whose triangles share vertex ids is a mesh, and many of
# the usual ST_* questions about the polygons it covers can be answered
# from the triangles directly: measures are sums over triangles, relations
# between groups follow from shared vertices and edges, and clipping by a
# rectangle keeps a mesh. Groups are given per triangle (a coverage) or as a
# table of triangle and group (overlapping groups), the same groups that
# tri_dissolve() turns back into polygons.

#' Neighbouring triangles
#'
#' The edge table of a triangle pool: for each triangle, the triangle on
#' the other side of each of its three edges.
#'
#' Two triangles are neighbours across an edge when both hold its two
#' vertex ids, in opposite order (as counter-clockwise triangles that share
#' an edge do). An edge on the outside of the mesh, or one used by more than
#' two triangles, has no neighbour. Run [merge_coincident()] first on a pool
#' whose triangles do not already share vertex ids.
#'
#' @param x A `wkpool_tri`.
#'
#' @returns An integer matrix with one row per triangle and three columns:
#'   column `k` is the triangle across the edge from corner `k` to corner
#'   `k + 1` (corner 3 to corner 1 for the third), `NA` where there is none.
#'
#' @examples
#' v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
#' tri <- new_wkpool_tri(v, vx0 = c(1L, 1L), vx1 = c(2L, 3L), vx2 = c(3L, 4L))
#' tri_neighbours(tri)
#'
#' @export
tri_neighbours <- function(x) {
  m <- tri_mesh(x)
  nb <- matrix(m$nbr, ncol = 3L, byrow = TRUE)
  nb[nb == 0L] <- NA_integer_
  nb
}

#' Measures of groups of triangles
#'
#' Area, perimeter, centroid, a point inside, and extent of each group of
#' triangles, computed from the triangles without forming polygons. These
#' are what `ST_Area()`, `ST_Perimeter()`, `ST_Centroid()`,
#' `ST_PointOnSurface()` and `ST_Envelope()` return for the polygons the
#' groups cover.
#'
#' The area is the sum of triangle areas and the centroid their
#' area-weighted mean. The perimeter is the length of the edges whose
#' other side is not in the group. The point inside is the centroid of the
#' group's largest triangle, so it is inside the group by construction.
#' Triangles must be counter-clockwise (a clockwise triangle counts
#' negative area).
#'
#' @inheritParams tri_dissolve
#' @param group Groups of triangles. Either one integer per triangle (a
#'   dense, 1-based group id, `NA` to leave a triangle out), or a data frame
#'   with columns `triangle` (row in `x`) and `group`, for groups that
#'   overlap (a triangle in several groups, as in an overlay). The default
#'   is the triangles' feature, or one group when the pool has no feature.
#'
#' @returns A data frame with one row per group id from 1 to the largest,
#'   and columns `group`, `area`, `perimeter`, `centroid_x`, `centroid_y`,
#'   `point_x`, `point_y`, `xmin`, `ymin`, `xmax`, `ymax`. A group with no
#'   triangles has zero area and perimeter and `NA` elsewhere.
#'
#' @seealso [tri_dissolve()] for the polygons themselves.
#'
#' @examples
#' v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
#' tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
#'                       vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
#' tri_measures(tri)
#' ## one group over both squares
#' tri_measures(tri, rep(1L, 4))
#'
#' @export
tri_measures <- function(x, group = NULL) {
  m <- tri_mesh(x)
  g <- tri_groups(x, group)
  r <- tri_measures_cpp(m$x, m$y, m$t0, m$t1, m$t2, m$nbr,
                        g$mem_start, g$mem, g$n)
  data.frame(group = seq_len(g$n), area = r[, 1], perimeter = r[, 2],
             centroid_x = r[, 3], centroid_y = r[, 4],
             point_x = r[, 5], point_y = r[, 6],
             xmin = r[, 7], ymin = r[, 8], xmax = r[, 9], ymax = r[, 10])
}

#' Relations between groups of triangles
#'
#' The DE-9IM matrix (as `ST_Relate()` returns it) between pairs of groups
#' of one triangle mesh, computed from shared triangles, edges and vertices
#' alone, without reading coordinates.
#'
#' Every point of the plane lies in exactly one open triangle, open edge or
#' vertex of the mesh, or outside it, and each of those lies wholly in the
#' interior, boundary or exterior of a group. A triangle is interior when
#' it is in the group. An edge is interior when the triangles on both sides
#' are, boundary when one is (outside the mesh is never in a group). A
#' vertex is interior when every triangle around it is in the group and it
#' is not on the outside of the mesh, exterior when none is, and boundary
#' otherwise. Each matrix entry is the largest dimension of a mesh element
#' in both classes: `"2"` for a triangle, `"1"` for an edge, `"0"` for a
#' vertex, `"F"` for none.
#'
#' The answer is exact for the mesh as given. It equals the relation of the
#' polygons the groups cover when the mesh was built so that every place
#' two inputs meet is a shared vertex, as a constrained triangulation that
#' resolves crossings does. Pairs of groups that share no vertex are
#' disjoint, `"FF2FF1212"`, and are not listed when `pairs` is `NULL`.
#'
#' @inheritParams tri_measures
#' @param pairs `NULL` for every pair of groups that share a vertex (all
#'   pairs that are not disjoint), or a two-column matrix or data frame of
#'   group ids.
#'
#' @returns A data frame with columns `a`, `b` (group ids) and `relate`
#'   (the nine-character DE-9IM string, interior, boundary, exterior of `a`
#'   by those of `b`).
#'
#' @examples
#' ## a 2 x 1 strip of unit squares and a group covering both
#' v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
#' tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
#'                       vx2 = c(3, 4, 6, 3))
#' g <- data.frame(triangle = c(1, 2, 3, 4, 1, 2, 3, 4),
#'                 group = c(1, 1, 2, 2, 3, 3, 3, 3))
#' tri_relate(tri, g)
#'
#' @export
tri_relate <- function(x, group = NULL, pairs = NULL) {
  m <- tri_mesh(x)
  g <- tri_groups(x, group)
  if (is.null(pairs)) {
    p <- tri_vertex_pairs_cpp(m$t0, m$t1, m$t2, g$mem_start, g$mem)
    a <- p[, 1]
    b <- p[, 2]
  } else {
    pairs <- as.matrix(pairs)
    if (ncol(pairs) != 2L) {
      stop("`pairs` must have two columns of group ids", call. = FALSE)
    }
    a <- as.integer(pairs[, 1])
    b <- as.integer(pairs[, 2])
    if (anyNA(a) || anyNA(b) || any(c(a, b) < 1L | c(a, b) > g$n)) {
      stop("`pairs` must hold group ids from 1 to the number of groups", call. = FALSE)
    }
  }
  r <- tri_relate_cpp(m$t0, m$t1, m$t2, length(m$x), m$nbr, g$mem_start, g$mem,
                      g$grp_start, g$grp_tri, a, b)
  data.frame(a = a, b = b, relate = r)
}

#' Boundary edges of groups of triangles
#'
#' The edges between a group and what is not in it, as a segment pool on
#' the triangles' vertex pool. This is `ST_Boundary()` of the polygons the
#' groups cover, with shared boundaries on shared vertices.
#'
#' Each edge is directed with its group on the left, so outer boundaries run
#' counter-clockwise and holes clockwise. With `once = TRUE` an edge between
#' two groups is kept for the smaller group id only, so every line is drawn
#' once (see [find_arcs()] to merge the edges into arcs).
#'
#' @inheritParams tri_measures
#' @param once Keep an edge shared by two groups only once?
#'
#' @returns A [wkpool] of segments with the group as its feature.
#'
#' @examples
#' v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
#' tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
#'                       vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
#' tri_boundary(tri)
#' ## the middle edge once
#' length(tri_boundary(tri, once = TRUE))
#'
#' @export
tri_boundary <- function(x, group = NULL, once = FALSE) {
  m <- tri_mesh(x)
  g <- tri_groups(x, group)
  b <- tri_boundary_cpp(m$t0, m$t1, m$t2, m$nbr, g$mem_start, g$mem, isTRUE(once))
  v <- pool_vertices(x)
  new_wkpool(v, v$.vx[b[, 2]], v$.vx[b[, 3]], feature = b[, 1],
             crs = attr(x, "crs", exact = TRUE),
             geodesic = attr(x, "geodesic", exact = TRUE))
}

#' Clip triangles by a rectangle
#'
#' Keep the parts of the triangles inside a rectangle, as triangles again:
#' `ST_ClipByBox2D()` that returns a mesh.
#'
#' Triangles inside the rectangle are kept as they are, triangles outside
#' are dropped, and each triangle that crosses a side is cut to a convex
#' polygon and fanned back into triangles. New vertices where edges cross
#' the sides are added to the vertex pool, and a point where a shared edge
#' crosses a side is computed the same way from both of its triangles, so
#' neighbouring pieces share it exactly. Slivers of zero area on a side are
#' dropped.
#'
#' @inheritParams tri_dissolve
#' @param rect The rectangle: a [wk::rct()] or `c(xmin, ymin, xmax, ymax)`.
#'
#' @returns A `wkpool_tri` with the input vertices plus the new ones
#'   (`origin` column `"clip"` for new vertices when the pool has an
#'   `origin` column; use [pool_compact()] to drop unused vertices), keeping
#'   the feature and part of each source triangle. The source triangle row
#'   of each output triangle is in attribute `"source"`.
#'
#' @examples
#' v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
#' tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
#'                       vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
#' clipped <- tri_clip_rect(tri, c(0.5, 0.25, 1.5, 0.75))
#' tri_measures(clipped)
#'
#' @export
tri_clip_rect <- function(x, rect) {
  m <- tri_mesh(x)
  if (inherits(rect, "wk_rct")) {
    r <- unclass(rect)
    rect <- c(r$xmin, r$ymin, r$xmax, r$ymax)
  }
  rect <- as.double(rect)
  if (length(rect) != 4L || anyNA(rect) || rect[3] < rect[1] || rect[4] < rect[2]) {
    stop("`rect` must be xmin, ymin, xmax, ymax with xmin <= xmax and ymin <= ymax",
         call. = FALSE)
  }
  r <- tri_clip_rect_cpp(m$x, m$y, m$t0, m$t1, m$t2, rect[1], rect[2], rect[3], rect[4])
  v <- pool_vertices(x)
  n_out <- nrow(r)
  id <- c(r[, 2], r[, 5], r[, 8])
  cx <- c(r[, 3], r[, 6], r[, 9])
  cy <- c(r[, 4], r[, 7], r[, 10])
  vx <- integer(length(id))
  old <- id > 0
  vx[old] <- v$.vx[id[old]]
  ## new points: pooled by exact coordinates
  if (any(!old)) {
    key <- data.frame(x = cx[!old], y = cy[!old])
    first <- !duplicated(key)
    new_xy <- key[first, , drop = FALSE]
    start <- if (nrow(v)) max(v$.vx) else 0L
    new_id <- start + seq_len(nrow(new_xy))
    vx[!old] <- new_id[match(paste(key$x, key$y), paste(new_xy$x, new_xy$y))]
    nv <- v[rep(NA_integer_, nrow(new_xy)), , drop = FALSE]
    nv$.vx <- as.integer(new_id)
    nv$x <- new_xy$x
    nv$y <- new_xy$y
    if ("origin" %in% names(nv)) nv$origin <- "clip"
    rownames(nv) <- NULL
    v <- rbind(v, nv)
  }
  src <- as.integer(r[, 1])
  out <- new_wkpool_tri(
    v,
    vx0 = vx[seq_len(n_out)], vx1 = vx[n_out + seq_len(n_out)],
    vx2 = vx[2L * n_out + seq_len(n_out)],
    feature = pool_feature(x)[src],
    part = tryCatch(vctrs::field(x, ".part"), error = function(e) NULL)[src],
    paths = attr(x, "paths", exact = TRUE),
    crs = attr(x, "crs", exact = TRUE),
    geodesic = attr(x, "geodesic", exact = TRUE)
  )
  attr(out, "source") <- src
  out
}

#' Convex hulls of groups of triangles
#'
#' `ST_ConvexHull()` of the polygons the groups cover, from the triangles'
#' vertices.
#'
#' @inheritParams tri_measures
#'
#' @returns A [wk::wkb()] vector of POLYGON, one per group id from 1 to the
#'   largest (`POLYGON EMPTY` for a group with fewer than three distinct
#'   points), with the pool's crs. Rings are counter-clockwise.
#'
#' @examples
#' v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
#' tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
#'                       vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
#' wk::as_wkt(tri_convex_hull(tri, rep(1L, 4)))
#'
#' @export
tri_convex_hull <- function(x, group = NULL) {
  m <- tri_mesh(x)
  g <- tri_groups(x, group)
  h <- tri_convex_hull_cpp(m$x, m$y, m$t0, m$t1, m$t2, g$grp_start, g$grp_tri, g$n)
  crs <- attr(x, "crs", exact = TRUE)
  empty <- unclass(wk::as_wkb(wk::wkt("POLYGON EMPTY")))[[1L]]
  out <- rep(list(empty), g$n)
  if (nrow(h)) {
    p <- wk::wk_polygon(wk::xy(m$x[h[, 2]], m$y[h[, 2]]), feature_id = h[, 1])
    geom <- unclass(wk::as_wkb(p))
    out[unique(h[, 1])] <- geom
  }
  wk::wkb(out, crs = crs)
}

#' Locate points in a triangle pool
#'
#' The triangle that holds each point: point in polygon (`ST_Intersects()`
#' or `ST_Contains()` of a point) answered by point location on the mesh.
#'
#' Triangles are bucketed on a uniform grid over the extent of the mesh,
#' and each candidate is tested with an exact orientation sign, so a point
#' on an edge or a vertex is decided the same way for every triangle that
#' holds it, and goes to the lowest triangle row among them. A point inside
#' no triangle, or with a missing coordinate, is `NA`. Triangles may be
#' either orientation; a degenerate triangle (zero area) holds no point.
#'
#' The feature of the polygon each point falls in is
#' `pool_feature(x)[tri_locate(x, at)]`, and the group of any grouping
#' (as in [tri_measures()]) is found the same way.
#'
#' @inheritParams tri_dissolve
#' @param at The points: any geometry [wk::wk_handle()] understands (its
#'   vertices are used, as [wk::wk_coords()] gives them), a two-column
#'   matrix, or a data frame or list with `x` and `y`.
#'
#' @returns An integer vector with one triangle row of `x` per point, `NA`
#'   where no triangle holds it.
#'
#' @examples
#' v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
#' tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
#'                       vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
#' pts <- wk::xy(c(0.75, 0.25, 1.5, 3), c(0.25, 0.75, 0.5, 0.5))
#' i <- tri_locate(tri, pts)
#' i
#' ## the feature each point falls in
#' pool_feature(tri)[i]
#'
#' @export
tri_locate <- function(x, at) {
  m <- tri_mesh(x)
  if (is.matrix(at)) {
    if (ncol(at) != 2L) stop("a matrix `at` must have two columns", call. = FALSE)
    px <- at[, 1]
    py <- at[, 2]
  } else if (is.list(at) && !inherits(at, c("wk_rcrd", "wk_vctr", "sfc")) &&
             !is.null(at$x) && !is.null(at$y)) {
    px <- at$x
    py <- at$y
  } else {
    xy <- wk::wk_coords(at)
    px <- xy$x
    py <- xy$y
  }
  if (!is.numeric(px) || !is.numeric(py) || length(px) != length(py)) {
    stop("`at` must give numeric x and y of the same length", call. = FALSE)
  }
  tri_locate_cpp(m$x, m$y, m$t0, m$t1, m$t2, as.double(px), as.double(py))
}

## ---- helpers ----

## triangle corners as rows of the vertex table, coordinates, edge table
tri_mesh <- function(x) {
  cache <- tri_cache(x)
  if (!is.null(cache)) return(cache$mesh)
  tri_mesh_build(x)
}

tri_mesh_build <- function(x) {
  check_wkpool_tri(x)
  v <- pool_vertices(x)
  t <- pool_triangles(x)
  t0 <- vx_match(t$.vx0, v$.vx)
  t1 <- vx_match(t$.vx1, v$.vx)
  t2 <- vx_match(t$.vx2, v$.vx)
  list(x = as.double(v$x), y = as.double(v$y), t0 = t0, t1 = t1, t2 = t2,
       nbr = tri_neighbours_cpp(t0, t1, t2))
}

## membership of triangles in groups, both ways, as offsets
tri_groups <- function(x, group = NULL) {
  cache <- tri_cache(x)
  if (!is.null(cache) && identical(group, cache$group)) return(cache$groups)
  tri_groups_build(x, group)
}

tri_groups_build <- function(x, group = NULL) {
  nt <- length(x)
  if (is.null(group)) {
    group <- pool_feature(x) %||% rep(1L, nt)
  }
  if (is.data.frame(group)) {
    if (!all(c("triangle", "group") %in% names(group))) {
      stop("a `group` data frame needs columns `triangle` and `group`", call. = FALSE)
    }
    tri <- group$triangle
    grp <- group$group
    if (anyNA(tri) || any(tri < 1 | tri > nt | tri != round(tri))) {
      stop("`group$triangle` must be rows of `x`", call. = FALSE)
    }
  } else {
    if (length(group) != nt) {
      stop("`group` must have one value per triangle, or be a data frame", call. = FALSE)
    }
    tri <- seq_len(nt)
    grp <- group
  }
  if (!is.numeric(grp) || any(!is.na(grp) & (grp < 1 | grp != round(grp)))) {
    stop("groups must be positive whole numbers or NA", call. = FALSE)
  }
  keep <- !is.na(grp)
  tri <- as.integer(tri[keep])
  grp <- as.integer(grp[keep])
  ng <- if (length(grp)) max(grp) else 0L
  ## unique (triangle, group) pairs
  o <- order(tri, grp)
  tri <- tri[o]
  grp <- grp[o]
  d <- !(duplicated(tri) & c(FALSE, diff(grp) == 0L))
  tri <- tri[d]
  grp <- grp[d]
  o2 <- order(grp, tri)
  list(n = ng,
       mem_start = as.integer(c(1L, cumsum(tabulate(tri, nt)) + 1L)),
       mem = grp,
       grp_start = as.integer(c(1L, cumsum(tabulate(grp, ng)) + 1L)),
       grp_tri = tri[o2])
}
