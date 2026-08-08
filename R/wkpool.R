# wkpool: vertex pool topology for wk
#
# Decision log:
#   - Pool scope: per-vector, sidecar attribute
#   - Snapping: observe don't correct (represent truth)
#   - Vertex identity: minted .vx integer (survives subset)
#   - Segment identity: derived from (.vx0, .vx1) pair
#   - Primary vctr: segments (geometry is what you subset)
#   - Vertices follow as attribute
#   - CRS: opaque, carried in the "crs" attribute per wk convention
#   - Geodesic: carried in the "geodesic" attribute per wk convention
#     (NULL when FALSE, otherwise TRUE or NA)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Mirror wk's storage convention for the geodesic attribute:
# FALSE is stored as NULL so that the attribute is absent in the
# common case; TRUE and NA (inherit) are stored as-is.
geodesic_attr <- function(geodesic) {
  if (!is.logical(geodesic) || (length(geodesic) != 1L)) {
    stop("`geodesic` must be TRUE, FALSE, or NA", call. = FALSE)
  }
  if (identical(geodesic, FALSE)) NULL else geodesic
}

# Constructor ------------------------------------------------------------

#' Construct a wkpool from validated components
#'
#' A low-level constructor for wkpool objects, for use by packages that
#' build pools directly (for example after refining or transforming an
#' existing pool). Inputs are checked against the wkpool invariants:
#' every segment endpoint must be a vertex id present in the pool.
#'
#' @param vertices A data.frame with columns `.vx` (integer vertex ids),
#'   `x`, `y`, and optionally `z`.
#' @param vx0,vx1 Integer vectors of segment start/end vertex ids, each
#'   value present in `vertices$.vx`.
#' @param feature Optional integer vector of feature ids, one per segment.
#' @param path Optional integer vector of path ids, one per segment. A
#'   path is a maximal run of segments minted from one input ring or
#'   linestring; it is the provenance that lets cycles, ring roles and
#'   feature structure be recovered exactly.
#' @param paths Optional data.frame describing the paths: columns
#'   `.path`, `.feature`, `.part`, `.ring` (as captured from
#'   [wk::wk_coords()] identifiers by [establish_topology()]).
#' @param crs A CRS object (commonly an authority string such as
#'   "EPSG:4326"), carried but never interpreted, per wk's CRS
#'   propagation model. Use `NULL` for none, or [wk::wk_crs_inherit()].
#' @param geodesic `TRUE` if segments should be interpreted as geodesics
#'   when coordinates are spherical, `FALSE` otherwise, `NA` to inherit.
#'
#' @returns A wkpool object.
#'
#' @examples
#' v <- data.frame(.vx = 1:3, x = c(0, 1, 1), y = c(0, 0, 1))
#' new_wkpool(v, vx0 = c(1L, 2L), vx1 = c(2L, 3L), crs = "EPSG:4326")
#'
#' @export
new_wkpool <- function(vertices, vx0, vx1, feature = NULL,
                       path = NULL, paths = NULL,
                       crs = NULL, geodesic = FALSE) {

  stopifnot(is.data.frame(vertices))
  stopifnot(".vx" %in% names(vertices))
  stopifnot(all(vx0 %in% vertices$.vx))
  stopifnot(all(vx1 %in% vertices$.vx))

  fields <- list(.vx0 = vx0, .vx1 = vx1)
  if (!is.null(feature)) {
    stopifnot(length(feature) == length(vx0))
    fields$.feature <- as.integer(feature)
  }
  if (!is.null(path)) {
    stopifnot(length(path) == length(vx0))
    fields$.path <- as.integer(path)
  }
  if (!is.null(paths)) {
    stopifnot(is.data.frame(paths))
    stopifnot(all(c(".path", ".feature", ".part", ".ring") %in% names(paths)))
    if (!is.null(path)) {
      stopifnot(all(path %in% paths$.path))
    }
  }

  vctrs::new_rcrd(
    fields,
    pool = vertices,
    paths = paths,
    crs = crs,
    geodesic = if (is.null(geodesic)) NULL else geodesic_attr(geodesic),
    class = "wkpool"
  )
}

# User constructor -------------------------------------------------------

wkpool <- function(vertices, segments, paths = NULL, crs = NULL, geodesic = FALSE) {
  #vctrs::vec_assert(segments, data.frame())
  if (!is.data.frame(segments)) stop("`segments` must be a data.frame")
  feature <- if (".feature" %in% names(segments)) segments$.feature else NULL
  path <- if (".path" %in% names(segments)) segments$.path else NULL
  new_wkpool(vertices, segments$.vx0, segments$.vx1, feature = feature,
             path = path, paths = paths, crs = crs, geodesic = geodesic)
}

# Empty pool -------------------------------------------------------------

wkpool_empty <- function(crs = wk::wk_crs_inherit(), geodesic = NA) {
  new_wkpool(
    vertices = data.frame(.vx = integer(), x = numeric(), y = numeric()),
    vx0 = integer(),
    vx1 = integer(),
    feature = integer(),
    crs = crs,
    geodesic = geodesic
  )
}

# CRS and geodesic -------------------------------------------------------

#' CRS and geodesic handling for wkpool
#'
#' A wkpool participates in wk's CRS propagation model: the CRS is an
#' opaque object carried in an attribute, never interpreted, and checked
#' for equality when pools are combined. Likewise the geodesic flag
#' records whether segments should be interpreted as geodesics when
#' coordinates are spherical. Both are captured from the input by
#' [establish_topology()], survive subsetting, merging and compaction,
#' and are restored onto geometry produced by [segments_to_wkt()],
#' [arcs_to_wkt()], [cycles_to_wkt()] and their WKB counterparts.
#'
#' @param x A wkpool object.
#' @param crs A CRS object (commonly an authority string such as
#'   "EPSG:4326"), or `NULL`.
#' @param geodesic `TRUE`, `FALSE`, or `NA` (inherit).
#'
#' @returns
#' - `wk_crs()`: the CRS object, or `NULL`.
#' - `wk_set_crs()`, `wk_set_geodesic()`: `x` with the attribute replaced.
#' - `wk_is_geodesic()`: `TRUE`, `FALSE`, or `NA`.
#'
#' @examples
#' x <- wk::wkt("LINESTRING (0 0, 1 1)", crs = "EPSG:4326")
#' pool <- establish_topology(x)
#' wk::wk_crs(pool)
#' wk::wk_crs(wk::wk_set_crs(pool, "EPSG:3031"))
#'
#' @name wkpool-crs
NULL

#' @rdname wkpool-crs
#' @exportS3Method wk::wk_crs
wk_crs.wkpool <- function(x) {
  attr(x, "crs", exact = TRUE)
}

#' @rdname wkpool-crs
#' @exportS3Method wk::wk_set_crs
wk_set_crs.wkpool <- function(x, crs) {
  attr(x, "crs") <- crs
  x
}

#' @rdname wkpool-crs
#' @exportS3Method wk::wk_is_geodesic
wk_is_geodesic.wkpool <- function(x) {
  attr(x, "geodesic", exact = TRUE) %||% FALSE
}

#' @rdname wkpool-crs
#' @exportS3Method wk::wk_set_geodesic
wk_set_geodesic.wkpool <- function(x, geodesic) {
  attr(x, "geodesic") <- geodesic_attr(geodesic)
  x
}

# Accessors --------------------------------------------------------------

#' Access components of a wkpool object
#'
#' Extract the vertex pool, segment table, or feature vector from a wkpool.
#'
#' @param x A wkpool object.
#'
#' @returns
#' - `pool_vertices()`: A data frame with columns `.vx` (vertex ID), `x`, `y`,
#'   and optionally `z`.
#' - `pool_segments()`: A data frame with columns `.vx0`, `.vx1`, and
#'   optionally `.feature` and `.path`.
#' - `pool_feature()`: An integer vector of feature IDs, or `NULL` if no
#'   feature information is present.
#' - `pool_path()`: An integer vector of path IDs (one per segment), or
#'   `NULL` if no path provenance is present.
#' - `pool_paths()`: A data frame describing each path (`.path`,
#'   `.feature`, `.part`, `.ring`, as captured from [wk::wk_coords()]
#'   identifiers), or `NULL`.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' pool_vertices(pool)
#' pool_segments(pool)
#' pool_feature(pool)
#'
#' @name wkpool-accessors
NULL

#' @rdname wkpool-accessors
#' @export
pool_vertices <- function(x) {
  attr(x, "pool")
}

#' @rdname wkpool-accessors
#' @export
pool_segments <- function(x) {
  check_wkpool(x)
  out <- data.frame(
    .vx0 = vctrs::field(x, ".vx0"),
    .vx1 = vctrs::field(x, ".vx1")
  )
  feat <- pool_feature(x)
  if (!is.null(feat)) {
    out$.feature <- feat
  }
  path <- pool_path(x)
  if (!is.null(path)) {
    out$.path <- path
  }
  out
}

#' @rdname wkpool-accessors
#' @export
pool_feature <- function(x) {
  tryCatch(
    vctrs::field(x, ".feature"),
    error = function(e) NULL
  )
}

#' @rdname wkpool-accessors
#' @export
pool_path <- function(x) {
  tryCatch(
    vctrs::field(x, ".path"),
    error = function(e) NULL
  )
}

#' @rdname wkpool-accessors
#' @export
pool_paths <- function(x) {
  attr(x, "paths", exact = TRUE)
}

# Format/print -----------------------------------------------------------

#' @export
format.wkpool <- function(x, ...) {
  n_seg <- length(x)
  n_vtx <- nrow(pool_vertices(x))
  sprintf("<segment: %d->%d>", vctrs::field(x, ".vx0"), vctrs::field(x, ".vx1"))
}

#' @export
vec_ptype_abbr.wkpool <- function(x, ...) "wkpl"

#' @export
obj_print_header.wkpool <- function(x, ...) {
  n_seg <- length(x)
  n_vtx <- nrow(pool_vertices(x))
  crs <- wk::wk_crs(x)
  crs_label <- if (is.null(crs) || inherits(crs, "wk_crs_inherit")) {
    ""
  } else {
    sprintf(" CRS=%s", format(crs)[1])
  }
  geo_label <- if (isTRUE(wk::wk_is_geodesic(x))) " geodesic" else ""
  cat(sprintf("<wkpool[%d segments, %d vertices]%s%s>\n",
              n_seg, n_vtx, crs_label, geo_label))
}

# vctrs boilerplate ------------------------------------------------------

#' @export
vec_ptype2.wkpool.wkpool <- function(x, y, ...) {
  wkpool_empty()
}

#' @export
vec_cast.wkpool.wkpool <- function(x, to, ...) {
  x
}

#' @export
vec_restore.wkpool <- function(x, to, ...) {
  # On subset: keep full pool and full paths table, just subset
  # segments; crs and geodesic come along from the original vector
  pool <- pool_vertices(to)
  feature <- tryCatch(vctrs::field(x, ".feature"), error = function(e) NULL)
  path <- tryCatch(vctrs::field(x, ".path"), error = function(e) NULL)
  new_wkpool(pool, vctrs::field(x, ".vx0"), vctrs::field(x, ".vx1"),
             feature = feature,
             path = path,
             paths = attr(to, "paths", exact = TRUE),
             crs = attr(to, "crs", exact = TRUE),
             geodesic = attr(to, "geodesic", exact = TRUE))
}

# Combine pools ----------------------------------------------------------

#' Combine wkpool objects
#'
#' @param ... wkpool objects to combine
#' @return A single wkpool with merged pools and remapped segments
#'
#' @examples
#' x <- wk::as_wkb("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
#' y <- wk::as_wkb("POLYGON ((2 0, 3 0, 3 1, 2 1, 2 0))")
#' pool_a <- establish_topology(x)
#' pool_b <- establish_topology(y)
#' pool_combine(pool_a, pool_b)
#'
#' @export
pool_combine <- function(...) {
  xs <- list(...)
  for (i in seq_along(xs)) {
    check_wkpool(xs[[i]], "x")
  }
  if (length(xs) == 0) return(wkpool_empty())
  if (length(xs) == 1) return(xs[[1]])

  # Resolve crs/geodesic across inputs using wk's propagation rules:
  # wk_crs_inherit()/NA give way to concrete values, unequal concrete
  # values are an error
  crs <- do.call(wk::wk_crs_output, xs)
  geodesic <- do.call(wk::wk_is_geodesic_output, xs)

  # Build new pool and remap tables: per-pool blocks of contiguous new
  # ids, segments remapped by position (match) plus the block offset -
  # no character keys, no growing vectors
  pools <- lapply(xs, pool_vertices)
  sizes <- vapply(pools, nrow, integer(1))
  offsets <- cumsum(c(0L, sizes))[seq_along(pools)]

  # Path provenance combines only when every input carries it; path ids
  # are offset per input so they stay unique
  all_paths <- !any(vapply(xs, function(p) is.null(pool_path(p)) || is.null(pool_paths(p)), logical(1)))
  path_offset <- 0L

  vx0_list <- vector("list", length(xs))
  vx1_list <- vector("list", length(xs))
  feature_list <- vector("list", length(xs))
  has_feature <- FALSE
  path_list <- vector("list", length(xs))
  new_paths <- vector("list", length(xs))

  for (i in seq_along(xs)) {
    old_ids <- pools[[i]]$.vx
    pools[[i]]$.vx <- offsets[i] + seq_len(sizes[i])

    old_vx0 <- vctrs::field(xs[[i]], ".vx0")
    old_vx1 <- vctrs::field(xs[[i]], ".vx1")
    vx0_list[[i]] <- offsets[i] + match(old_vx0, old_ids)
    vx1_list[[i]] <- offsets[i] + match(old_vx1, old_ids)

    feat <- pool_feature(xs[[i]])
    if (!is.null(feat)) {
      has_feature <- TRUE
      feature_list[[i]] <- feat
    } else {
      feature_list[[i]] <- rep(NA_integer_, length(old_vx0))
    }

    if (all_paths) {
      pt <- pool_paths(xs[[i]])
      path_list[[i]] <- pool_path(xs[[i]]) + path_offset
      pt$.path <- pt$.path + path_offset
      new_paths[[i]] <- pt
      path_offset <- path_offset + max(pt$.path, 0L)
    }
  }

  new_pool <- vctrs::vec_rbind(!!!pools)

  new_wkpool(new_pool, unlist(vx0_list), unlist(vx1_list),
             feature = if (has_feature) unlist(feature_list) else NULL,
             path = if (all_paths) unlist(path_list) else NULL,
             paths = if (all_paths) vctrs::vec_rbind(!!!new_paths) else NULL,
             crs = crs, geodesic = geodesic)
}

#' Combine many wkpool vectors into one vector
#'
#' This is non-functional, wkpool does not currently support [vec_c()].
#'
#' Attempts to combine wkpool vectors with vec_c will suggest
#' using [pool_combine()' instead.
#' @inheritParams vctrs::vec_c
#' @export
#' @importFrom vctrs vec_c
#' @exportS3Method vctrs::vec_c
#' @name vec_c
#' @return nothing, used for a message side-effect (see Details)
#' @seealso [pool_combine()]
vec_c.wkpool <- function(..., .ptype = NULL) {
  stop(
    "Use pool_combine() to combine wkpool objects.\n",
    "vec_c() is not supported yet.",
    call. = FALSE
  )
}

# Plotting ---------------------------------------------------------------

#' Plot a wkpool object
#'
#' Draws segments coloured by feature membership.
#'
#' @param x A wkpool object.
#' @param col Colour(s) for segments. If `NULL` (default), segments are
#'   coloured by feature using a built-in palette.
#' @param ... Further arguments passed to [plot.default()].
#'
#' @returns Invisibly returns `x`.
#'
#' @examples
#' x <- wk::as_wkb(c(
#'   "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'   "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
#' ))
#' pool <- establish_topology(x)
#' plot(pool)
#'
#' @export
plot.wkpool <- function(x, col = NULL, ...) {
  check_wkpool(x)
  v <- pool_vertices(x)
  segs <- pool_segments(x)

  # Default color by feature if available
  if (is.null(col)) {
    feat <- pool_feature(x)
    if (!is.null(feat)) {
      pal <- c("steelblue", "coral", "forestgreen", "purple", "orange",
               "darkred", "darkgreen", "navy")
      col <- pal[(feat - 1) %% length(pal) + 1]
    } else {
      col <- "steelblue"
    }
  }

  plot(v$x, v$y, type = "n", ...)

  idx0 <- match(segs$.vx0, v$.vx)
  idx1 <- match(segs$.vx1, v$.vx)
  segments(v$x[idx0], v$y[idx0], v$x[idx1], v$y[idx1], col = col)

  invisible(x)
}





# Validation helper for wkpool
#
# Add this to wkpool.R (or a new validate.R file)

#' Check if input is a wkpool, with helpful error
#' @param x Input to check
#' @param arg Name of argument for error message
#' @noRd
check_wkpool <- function(x, arg = "x") {
  if (!inherits(x, "wkpool")) {
    stop(
      sprintf(
        "`%s` must be a wkpool object.\n
Use `establish_topology()` first, then optionally `merge_coincident()`.\n
Example:\n
  pool <- establish_topology(x)\n
  merged <- merge_coincident(pool)",
        arg
      ),
      call. = FALSE
    )
  }
  invisible(x)
}

