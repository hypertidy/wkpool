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

#' @export
pool_vertices <- function(x) {
  attr(x, "pool")
}

#' @export
pool_segments <- function(x) {
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
#' @export
pool_combine <- function(...) {
  xs <- list(...)
  
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

#' @export
plot.wkpool <- function(x, col = NULL, ...) {
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
