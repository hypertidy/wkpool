# tri_overlay(): two layers (polygon meshes, lines, points) in one mesh, so
# that relations between them come from the indices as they do within one
# mesh.

#' Overlay two layers in one mesh
#'
#' Put the features of two layers into one triangulation in which every
#' place they meet is a shared vertex or edge, so that relations between
#' them can be read from the mesh alone (see [tri_overlay_relate()]).
#'
#' A layer is one of:
#'
#' * a `wkpool_tri`, for polygons: groups of its triangles, as in
#'   [tri_measures()];
#' * a [wkpool] of segments, for lines: one line per feature (one line in
#'   all when the pool has no feature);
#' * any geometry that wk can read holding points or lines (not polygons;
#'   mesh those first), one feature per input geometry.
#'
#' The boundary edges of every polygon group and every line segment become
#' constraints of one constrained Delaunay triangulation of all the
#' layers' vertices and points ([cdtr::cdt_triangulate()]). Where
#' constraints cross, the crossing point is added as a vertex (computed in
#' floating point, so a relation that hinges on a crossing point is exact
#' for the overlay rather than for the inputs). Each triangle of the
#' overlay then belongs to the polygon groups that hold its centroid in the
#' source mesh, and each line is followed along the overlay's fixed edges.
#' The centroid is that of the largest triangle among those joined to it
#' without crossing a constraint (they all lie in the same groups), so that
#' slivers along a constraint are labelled from a triangle well inside.
#'
#' @param x,y Layers, as above. `y` may be `NULL` to put one layer alone
#'   into a mesh (lines or points have no mesh of their own).
#' @param x_group,y_group Groups for a polygon layer (as `group` in
#'   [tri_measures()]); ignored for lines and points.
#'
#' @returns A `wkpool_overlay`: a list with
#'   * `mesh`, the overlay as a `wkpool_tri` (vertex column `origin` is
#'     `"input"` or `"crossing"`);
#'   * `groups`, a data frame with one row per group of the overlay:
#'     `layer` (`"x"` or `"y"`), `id` (the group id in its layer), `dim` (2
#'     polygons, 1 lines, 0 points);
#'   * `members`, a data frame of `group` (row of `groups`), `item` and
#'     `item2`: a triangle row of `mesh` for polygons, the two vertex rows
#'     of an edge for lines, a vertex row for points (`item2` is `NA`
#'     except for lines).
#'
#' @seealso [tri_overlay_relate()]
#'
#' @examples
#' if (requireNamespace("cdtr", quietly = TRUE)) {
#'   ## a unit square as two triangles, and a line across it
#'   v <- data.frame(.vx = 1:4, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
#'   sq <- new_wkpool_tri(v, vx0 = c(1, 1), vx1 = c(2, 3), vx2 = c(3, 4))
#'   ln <- wk::wkt(c("LINESTRING (-1 0.5, 2 0.5)", "LINESTRING (2 2, 3 3)"))
#'   ov <- tri_overlay(sq, ln)
#'   ov$groups
#'   tri_overlay_relate(ov)
#' }
#'
#' @export
tri_overlay <- function(x, y = NULL, x_group = NULL, y_group = NULL) {
  if (!requireNamespace("cdtr", quietly = TRUE)) {
    stop("tri_overlay() requires the 'cdtr' package", call. = FALSE)
  }
  layers <- list(overlay_layer(x, x_group, "x"))
  if (!is.null(y)) layers <- c(layers, list(overlay_layer(y, y_group, "y")))
  crs <- overlay_crs(layers)

  ## all input coordinates, and constraints as rows of them
  offset <- 0L
  px <- double()
  py <- double()
  s0 <- integer()
  s1 <- integer()
  for (i in seq_along(layers)) {
    l <- layers[[i]]
    layers[[i]]$offset <- offset
    px <- c(px, l$x)
    py <- c(py, l$y)
    s0 <- c(s0, l$s0 + offset)
    s1 <- c(s1, l$s1 + offset)
    offset <- offset + length(l$x)
  }
  keep <- px[s0] != px[s1] | py[s0] != py[s1]
  s0 <- s0[keep]
  s1 <- s1[keep]
  cdt <- cdtr::cdt_triangulate(px, py, s0, s1, erase = "hull", intersect = "resolve")
  P <- cdt$P
  Tr <- cdt$T
  nv <- nrow(P)
  if (nrow(Tr) == 0L) stop("the layers do not span a triangle", call. = FALSE)
  origin <- rep("input", nv)
  if (cdt$n_crossing > 0) origin[cdt$n_input + seq_len(cdt$n_crossing)] <- "crossing"
  verts <- data.frame(.vx = seq_len(nv), x = P[, 1], y = P[, 2], origin = origin)
  mesh <- new_wkpool_tri(verts, Tr[, 1], Tr[, 2], Tr[, 3], crs = crs)
  mesh <- tri_prepare(mesh, rings = FALSE)
  ## each overlay triangle takes the polygon groups of its region, read
  ## from the centroid of the region's largest triangle
  rep_tri <- overlay_regions_cpp(P[, 1], P[, 2], Tr[, 1], Tr[, 2], Tr[, 3],
                                 tri_neighbours_cpp(Tr[, 1], Tr[, 2], Tr[, 3]),
                                 cdt$S[, 1], cdt$S[, 2])
  cx <- (P[Tr[, 1], 1] + P[Tr[, 2], 1] + P[Tr[, 3], 1]) / 3
  cy <- (P[Tr[, 1], 2] + P[Tr[, 2], 2] + P[Tr[, 3], 2]) / 3
  ctr <- cbind(cx, cy)[rep_tri, , drop = FALSE]

  groups <- list()
  members <- list()
  base <- 0L
  for (l in layers) {
    if (l$dim == 2L) {
      ## each overlay triangle takes the groups of the source triangle
      ## holding its centroid
      src <- tri_locate(l$source, ctr)
      g <- l$groups
      hit <- which(!is.na(src))
      n_per <- diff(g$mem_start)[src[hit]]
      tri <- rep(hit, n_per)
      from <- unlist(lapply(src[hit][n_per > 0], function(s) {
        seq.int(g$mem_start[s], length.out = g$mem_start[s + 1L] - g$mem_start[s])
      }), use.names = FALSE)
      grp <- g$mem[from]
      members[[length(members) + 1L]] <- data.frame(
        group = base + grp, item = tri, item2 = NA_integer_)
      ng <- g$n
    } else if (l$dim == 1L) {
      a <- cdt$input_map[l$seg0 + l$offset]
      b <- cdt$input_map[l$seg1 + l$offset]
      w <- overlay_walk_cpp(P[, 1], P[, 2], cdt$S[, 1], cdt$S[, 2], a, b)
      if (any(w[, 2] == 0L)) {
        stop("a line could not be followed through the overlay", call. = FALSE)
      }
      members[[length(members) + 1L]] <- data.frame(
        group = base + l$seg_group[w[, 1]], item = w[, 2], item2 = w[, 3])
      ng <- l$n
    } else {
      members[[length(members) + 1L]] <- data.frame(
        group = base + l$pt_group,
        item = cdt$input_map[l$offset + seq_along(l$pt_group)],
        item2 = NA_integer_)
      ng <- l$n
    }
    groups[[length(groups) + 1L]] <- data.frame(
      layer = rep(l$name, ng), id = seq_len(ng), dim = rep(l$dim, ng))
    base <- base + ng
  }
  members <- do.call(rbind, members)
  members <- members[order(members$group), , drop = FALSE]
  rownames(members) <- NULL
  structure(list(mesh = mesh, groups = do.call(rbind, groups), members = members),
            class = "wkpool_overlay")
}

#' Relations between the layers of an overlay
#'
#' The DE-9IM matrix (as `ST_Relate()` returns it) between features of the
#' two layers of a [tri_overlay()], or between features of its one layer,
#' read from the overlay mesh's triangles, edges and vertices.
#'
#' Every point of the plane is in one open triangle, open edge or vertex of
#' the mesh, or outside it, and each of those is in the interior, boundary
#' or exterior of a feature. For polygons the classes are those of
#' [tri_relate()]. A line's interior is its edges and the vertices inside
#' it, its boundary the vertices where an odd number of its edges end (the
#' mod-2 rule that GEOS uses by default). A point set's interior is its
#' vertices and it has no boundary. Each entry is the largest dimension of
#' a cell in both classes.
#'
#' @param x A `wkpool_overlay` from [tri_overlay()].
#' @param pairs `NULL` for every pair of features (one from each layer, or
#'   two of the one layer) whose closures share a vertex, the pairs that
#'   are not disjoint; or a two-column matrix or data frame of feature ids,
#'   the first in layer `x` and the second in layer `y` (or both in `x`
#'   for a single layer).
#'
#' @returns A data frame with columns `x` and `y` (feature ids in their
#'   layers) and `relate` (the nine-character DE-9IM string, interior,
#'   boundary and exterior of the first by those of the second).
#'
#' @inherit tri_overlay examples
#'
#' @export
tri_overlay_relate <- function(x, pairs = NULL) {
  if (!inherits(x, "wkpool_overlay")) {
    stop("`x` must be a wkpool_overlay from tri_overlay()", call. = FALSE)
  }
  g <- x$groups
  in_x <- which(g$layer == "x")
  in_y <- which(g$layer == "y")
  single <- length(in_y) == 0L
  if (single) in_y <- in_x
  if (is.null(pairs)) {
    p <- overlay_touching(x, in_x, in_y)
    if (single) p <- p[p[, 1] < p[, 2], , drop = FALSE]
    a <- p[, 1]
    b <- p[, 2]
  } else {
    pairs <- as.matrix(pairs)
    if (ncol(pairs) != 2L) stop("`pairs` must have two columns of feature ids", call. = FALSE)
    ia <- as.integer(pairs[, 1])
    ib <- as.integer(pairs[, 2])
    if (anyNA(ia) || anyNA(ib) || any(ia < 1L | ia > length(in_x)) ||
        any(ib < 1L | ib > length(in_y))) {
      stop("`pairs` must hold feature ids of the layers", call. = FALSE)
    }
    a <- in_x[ia]
    b <- in_y[ib]
  }
  m <- tri_mesh(x$mesh)
  mem <- x$members
  g_start <- as.integer(c(1L, cumsum(tabulate(mem$group, nrow(g))) + 1L))
  item2 <- mem$item2
  item2[is.na(item2)] <- 0L
  r <- overlay_relate_cpp(m$t0, m$t1, m$t2, length(m$x), m$nbr,
                          as.integer(g$dim), g_start, as.integer(mem$item),
                          as.integer(item2), as.integer(a), as.integer(b))
  data.frame(x = g$id[a], y = g$id[b], relate = r)
}

## ---- helpers ----

## the crs of the layers: either may be unset, but two must agree
overlay_crs <- function(layers) {
  crs <- lapply(layers, function(l) l$crs)
  crs <- crs[!vapply(crs, is.null, logical(1))]
  if (length(crs) == 2L && !wk::wk_crs_equal(crs[[1]], crs[[2]])) {
    stop("the layers have different crs", call. = FALSE)
  }
  if (length(crs)) crs[[1]] else NULL
}

#' @export
print.wkpool_overlay <- function(x, ...) {
  g <- x$groups
  cat("<wkpool_overlay> ", length(x$mesh), " triangles, ",
      nrow(pool_vertices(x$mesh)), " vertices\n", sep = "")
  for (l in unique(g$layer)) {
    d <- unique(g$dim[g$layer == l])
    cat("  layer ", l, ": ", sum(g$layer == l), " ",
        c("point", "line", "polygon")[d + 1L], " features\n", sep = "")
  }
  invisible(x)
}

## one layer as coordinates, constraints and what its groups are made of
overlay_layer <- function(x, group, name) {
  if (inherits(x, "wkpool_tri")) {
    g <- tri_groups(x, group)
    b <- tri_boundary(x, group, once = TRUE)
    v <- pool_vertices(b)
    s <- pool_segments(b)
    i0 <- vx_match(s$.vx0, v$.vx)
    i1 <- vx_match(s$.vx1, v$.vx)
    used <- sort(unique(c(i0, i1)))
    return(list(name = name, dim = 2L, n = g$n, x = as.double(v$x[used]),
                y = as.double(v$y[used]), s0 = match(i0, used), s1 = match(i1, used),
                source = x, groups = g, crs = attr(x, "crs", exact = TRUE)))
  }
  if (inherits(x, "wkpool")) {
    v <- pool_vertices(x)
    s <- pool_segments(x)
    f <- pool_feature(x) %||% rep(1L, nrow(s))
    i0 <- vx_match(s$.vx0, v$.vx)
    i1 <- vx_match(s$.vx1, v$.vx)
    return(list(name = name, dim = 1L, n = max(c(0L, f)), x = as.double(v$x),
                y = as.double(v$y), s0 = i0, s1 = i1, seg0 = i0, seg1 = i1,
                seg_group = as.integer(f), crs = attr(x, "crs", exact = TRUE)))
  }
  meta <- wk::wk_meta(x)
  type <- unique(meta$geometry_type)
  n <- length(type)
  co <- wk::wk_coords(x)
  nf <- nrow(meta)
  crs <- wk::wk_crs(x)
  if (all(type %in% c(1L, 4L))) {
    return(list(name = name, dim = 0L, n = nf, x = as.double(co$x),
                y = as.double(co$y), s0 = integer(), s1 = integer(),
                pt_group = as.integer(co$feature_id), crs = crs))
  }
  if (all(type %in% c(2L, 5L))) {
    k <- nrow(co)
    same <- co$feature_id[-1L] == co$feature_id[-k] &
      co$part_id[-1L] == co$part_id[-k]
    i0 <- which(same)
    i1 <- i0 + 1L
    return(list(name = name, dim = 1L, n = nf, x = as.double(co$x),
                y = as.double(co$y), s0 = i0, s1 = i1, seg0 = i0, seg1 = i1,
                seg_group = as.integer(co$feature_id[i0]), crs = crs))
  }
  if (n == 0L) stop("layer `", name, "` is empty", call. = FALSE)
  stop("layer `", name, "` must be a wkpool_tri (polygons), a wkpool (lines), ",
       "or points or lines that wk can read", call. = FALSE)
}

## pairs of groups (rows of x$groups) whose closures share a vertex
overlay_touching <- function(x, in_x, in_y) {
  m <- tri_mesh(x$mesh)
  mem <- x$members
  dim <- x$groups$dim[mem$group]
  corner <- cbind(m$t0, m$t1, m$t2)
  v <- ifelse(dim == 2L, NA_integer_, mem$item)
  vg <- data.frame(group = c(mem$group, mem$group[dim == 1L]),
                   vertex = c(v, mem$item2[dim == 1L]))
  pt <- mem[dim == 2L, , drop = FALSE]
  vg <- rbind(vg[!is.na(vg$vertex), ],
              data.frame(group = rep(pt$group, 3L),
                         vertex = c(corner[pt$item, 1], corner[pt$item, 2],
                                    corner[pt$item, 3])))
  nv <- length(m$x)
  key <- unique((vg$group - 1) * nv + vg$vertex)
  grp <- as.integer((key - 1) %/% nv) + 1L
  vtx <- as.integer((key - 1) %% nv) + 1L
  ng <- nrow(x$groups)
  overlay_pairs_cpp(vtx, grp, nv, seq_len(ng) %in% in_x, seq_len(ng) %in% in_y)
}
