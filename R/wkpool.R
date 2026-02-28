# wkpool: vertex pool topology for wk
#
# Decision log:
#   - Pool scope: per-vector, sidecar attribute
#   - Snapping: observe don't correct (represent truth)
#   - Vertex identity: minted .vx integer (survives subset)
#   - Segment identity: derived from (.vx0, .vx1) pair
#   - Primary vctr: segments (geometry is what you subset)
#   - Vertices follow as attribute

# Constructor (internal) -------------------------------------------------

new_wkpool <- function(vertices, vx0, vx1, feature = NULL) {

  stopifnot(is.data.frame(vertices))
  stopifnot(".vx" %in% names(vertices))
  stopifnot(all(vx0 %in% vertices$.vx))
  stopifnot(all(vx1 %in% vertices$.vx))

  fields <- list(.vx0 = vx0, .vx1 = vx1)
  if (!is.null(feature)) {
    stopifnot(length(feature) == length(vx0))
    fields$.feature <- as.integer(feature)
  }

  vctrs::new_rcrd(
    fields,
    pool = vertices,
    class = "wkpool"
  )
}

# User constructor -------------------------------------------------------

wkpool <- function(vertices, segments) {
  vctrs::vec_assert(segments, data.frame())
  feature <- if (".feature" %in% names(segments)) segments$.feature else NULL
  new_wkpool(vertices, segments$.vx0, segments$.vx1, feature = feature)
}

# Empty pool -------------------------------------------------------------

wkpool_empty <- function() {
  new_wkpool(
    vertices = data.frame(.vx = integer(), x = numeric(), y = numeric()),
    vx0 = integer(),
    vx1 = integer(),
    feature = integer()
  )
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
#'   optionally `.feature`.
#' - `pool_feature()`: An integer vector of feature IDs, or `NULL` if no
#'   feature information is present.
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
  cat(sprintf("<wkpool[%d segments, %d vertices]>\n", n_seg, n_vtx))
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
  # On subset: keep full pool, just subset segments
  pool <- pool_vertices(to)
  feature <- tryCatch(vctrs::field(x, ".feature"), error = function(e) NULL)
  new_wkpool(pool, vctrs::field(x, ".vx0"), vctrs::field(x, ".vx1"), feature = feature)
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

  # Build new pool and remap tables
  pools <- lapply(xs, pool_vertices)

  # For each pool, we need to remap old .vx -> new .vx
  remaps <- vector("list", length(pools))
  new_vx_start <- 1L

  for (i in seq_along(pools)) {
    old_vx <- pools[[i]]$.vx
    n <- length(old_vx)
    new_vx <- seq.int(new_vx_start, length.out = n)

    remaps[[i]] <- new_vx
    names(remaps[[i]]) <- as.character(old_vx)

    pools[[i]]$.vx <- new_vx
    new_vx_start <- new_vx_start + n
  }

  new_pool <- vctrs::vec_rbind(!!!pools)

  # Remap segment indices and collect features
  new_vx0 <- integer()
  new_vx1 <- integer()
  new_feature <- integer()
  has_feature <- FALSE

  for (i in seq_along(xs)) {
    old_vx0 <- vctrs::field(xs[[i]], ".vx0")
    old_vx1 <- vctrs::field(xs[[i]], ".vx1")
    feat <- pool_feature(xs[[i]])

    new_vx0 <- c(new_vx0, unname(remaps[[i]][as.character(old_vx0)]))
    new_vx1 <- c(new_vx1, unname(remaps[[i]][as.character(old_vx1)]))

    if (!is.null(feat)) {
      has_feature <- TRUE
      new_feature <- c(new_feature, feat)
    } else {
      new_feature <- c(new_feature, rep(NA_integer_, length(old_vx0)))
    }
  }

  new_wkpool(new_pool, new_vx0, new_vx1,
             feature = if (has_feature) new_feature else NULL)
}

#' @export
vec_c.wkpool <- function(..., .ptype = NULL) {
 # vctrs may not dispatch here for rcrd - use pool_combine() directly if issues
  pool_combine(...)
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

