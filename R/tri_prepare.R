# tri_prepare(): compute the derived tables of a triangle pool once and keep
# them with it, so that repeated tri_*() calls read them instead.

#' Prepare a triangle pool for repeated questions
#'
#' Compute once, and keep with the pool, what the `tri_*()` functions
#' otherwise derive on every call: the triangles as rows of the vertex
#' table, the edge table ([tri_neighbours()]), the membership of triangles
#' in groups, and the boundary rings of each group that [tri_dissolve()]
#' and [tri_structure()] read. This is the mesh counterpart of a prepared
#' geometry in GEOS.
#'
#' The cache is an attribute of the returned pool. It is used only while
#' the pool's vertices and triangles are unchanged (it is checked on every
#' use) and only for the `group` it was prepared with; any other `group`
#' is computed as usual. Subsetting or combining pools drops it.
#'
#' @inheritParams tri_measures
#' @param rings Also compute the boundary rings of each group? Only for a
#'   `group` given per triangle (not a table of overlapping groups).
#'
#' @returns `x`, with the cache attached.
#'
#' @examples
#' v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
#' tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
#'                       vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
#' p <- tri_prepare(tri)
#' tri_is_prepared(p)
#' tri_measures(p)
#' tri_structure(p)
#'
#' @export
tri_prepare <- function(x, group = NULL, rings = TRUE) {
  check_wkpool_tri(x)
  attr(x, "tri_cache") <- NULL
  mesh <- tri_mesh_build(x)
  groups <- tri_groups_build(x, group)
  ring_group <- NULL
  ring_tab <- NULL
  if (isTRUE(rings) && !is.data.frame(group)) {
    ring_group <- as.integer(group %||% pool_feature(x) %||% rep(1L, length(x)))
    keep <- !is.na(ring_group)
    ring_tab <- if (any(keep)) {
      dissolve_rings(mesh$t0[keep], mesh$t1[keep], mesh$t2[keep],
                     ring_group[keep], mesh$x, mesh$y)
    } else {
      data.frame(x = double(), y = double(), ring = integer(), poly = integer(),
                 group = integer())
    }
  }
  attr(x, "tri_cache") <- list(key = tri_cache_key(x), mesh = mesh,
                               group = group, groups = groups,
                               ring_group = ring_group, rings = ring_tab)
  x
}

#' @rdname tri_prepare
#' @export
tri_is_prepared <- function(x) {
  !is.null(tri_cache(x))
}

#' Parts and holes of groups of triangles
#'
#' The number of polygons (parts) and holes of each group, as
#' `ST_NumGeometries()` and `ST_NumInteriorRings()` (summed over parts)
#' give them for the dissolved groups. These need the boundary rings, so
#' they are a read on a pool from [tri_prepare()] and a dissolve otherwise.
#'
#' @inheritParams tri_dissolve
#'
#' @returns A data frame with one row per group id from 1 to the largest
#'   and columns `group`, `n_parts`, `n_holes` and `n_rings`.
#'
#' @inherit tri_prepare examples
#'
#' @export
tri_structure <- function(x, group = NULL) {
  check_wkpool_tri(x)
  nt <- length(x)
  if (is.null(group)) group <- pool_feature(x) %||% rep(1L, nt)
  if (length(group) != nt) {
    stop("`group` must have one value per triangle", call. = FALSE)
  }
  if (!is.numeric(group) || any(!is.na(group) & (group < 1 | group != round(group)))) {
    stop("`group` must be positive whole numbers or NA", call. = FALSE)
  }
  group <- as.integer(group)
  ng <- if (all(is.na(group))) 0L else max(group, na.rm = TRUE)
  out <- data.frame(group = seq_len(ng), n_parts = integer(ng),
                    n_holes = integer(ng), n_rings = integer(ng))
  if (ng == 0L || all(is.na(group))) return(out)
  r <- tri_group_rings(x, group)
  first <- !duplicated(r$ring)
  rg <- r$group[first]
  outer <- !duplicated(r$poly[first])
  out$n_rings <- tabulate(rg, ng)
  out$n_parts <- tabulate(rg[outer], ng)
  out$n_holes <- out$n_rings - out$n_parts
  out
}

## ---- helpers ----

tri_cache_key <- function(x) {
  list(pool = attr(x, "pool", exact = TRUE),
       vx0 = vctrs::field(x, ".vx0"), vx1 = vctrs::field(x, ".vx1"),
       vx2 = vctrs::field(x, ".vx2"))
}

## the cache, when it is there and still describes x
tri_cache <- function(x) {
  cache <- attr(x, "tri_cache", exact = TRUE)
  if (is.null(cache) || !identical(cache$key, tri_cache_key(x))) return(NULL)
  cache
}
