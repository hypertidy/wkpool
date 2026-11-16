# tri_as_arrow(), tri_from_arrow(): a triangle pool as an Arrow array of
# triangles that index one shared vertex dictionary, either one row per
# triangle or one row per feature (a list of the feature's triangles).

#' Triangles as an Arrow array
#'
#' Write a triangle pool as an Arrow array, built with nanoarrow, in the
#' layout of the experimental `geoarrow.triangles` type: each triangle is
#' three indices into one vertex dictionary (a struct of `x` and `y`) that
#' every triangle shares, so a vertex used by many triangles is stored once
#' and a shared boundary is a shared index.
#'
#' With `layout = "feature"` (the default) each row is one group (a
#' polygonal feature) and its value is the list of the group's triangles:
#' `List<FixedSizeList<Dictionary<int32, Coordinate>>[3]>`. The list
#' offsets are then the feature-to-triangle ranges, so an engine that
#' evaluates `ST_*` per row needs no grouping column. A triangle in several
#' groups is written in each of their rows (three more indices, no new
#' vertices). With `layout = "triangle"` each row is one triangle
#' (`FixedSizeList<Dictionary<int32, Coordinate>>[3]`), in pool order, the
#' storage of the mesh itself.
#'
#' Vertices are numbered by first appearance in the written triangles, and
#' vertices no triangle uses are not written. The extension metadata holds
#' `"edges": "planar"` and the pool's crs (as a string) when it has one.
#' The `geoarrow.triangles` type is a draft and not part of GeoArrow; the
#' layout may change.
#'
#' @inheritParams tri_measures
#' @param layout `"feature"` (one row per group) or `"triangle"` (one row per
#'   triangle; `group` is ignored).
#'
#' @returns A `nanoarrow_array`.
#'
#' @seealso [tri_from_arrow()] to read it back.
#'
#' @examples
#' if (requireNamespace("nanoarrow", quietly = TRUE)) {
#'   v <- data.frame(.vx = 1:6, x = c(0, 1, 1, 0, 2, 2), y = c(0, 0, 1, 1, 0, 1))
#'   tri <- new_wkpool_tri(v, vx0 = c(1, 1, 2, 2), vx1 = c(2, 3, 5, 6),
#'                         vx2 = c(3, 4, 6, 3), feature = c(1, 1, 2, 2))
#'   a <- tri_as_arrow(tri)
#'   a$length
#'   back <- tri_from_arrow(a)
#'   tri_measures(back$mesh, back$group)
#' }
#'
#' @export
tri_as_arrow <- function(x, group = NULL, layout = c("feature", "triangle")) {
  check_nanoarrow()
  layout <- match.arg(layout)
  m <- tri_mesh(x)
  nt <- length(x)
  if (layout == "feature") {
    g <- tri_groups(x, group)
    tri <- g$grp_tri
    offsets <- g$grp_start - 1L
  } else {
    tri <- seq_len(nt)
    offsets <- NULL
  }
  idx <- rbind(m$t0[tri], m$t1[tri], m$t2[tri])
  flat <- as.vector(idx)
  first <- unique(flat)
  code <- match(flat, first) - 1L
  crs <- attr(x, "crs", exact = TRUE)
  meta <- list(edges = "planar")
  if (!is.null(crs)) meta$crs <- as.character(wk::wk_crs_proj_definition(crs))
  tri_arrow_array(m$x[first], m$y[first], code, offsets, meta)
}

#' Read triangles from an Arrow array
#'
#' Read an array written by [tri_as_arrow()] (or anything with the same
#' storage: one shared dictionary of `x`, `y` coordinates indexed by
#' fixed-size lists of three, optionally inside a list per feature) back
#' into a triangle pool and its groups.
#'
#' A triangle that appears in several rows (a triangle in several features)
#' becomes one triangle of the pool, in several groups.
#'
#' @param x A `nanoarrow_array`, or anything [nanoarrow::as_nanoarrow_array()]
#'   accepts (such as an Arrow `Array`).
#'
#' @returns A list with `mesh`, a `wkpool_tri` (with the feature of each
#'   triangle when every triangle is in one row), and `group`, a data frame
#'   of `triangle` (row of `mesh`) and `group` (row of the array), or `NULL`
#'   for the one-row-per-triangle layout.
#'
#' @inherit tri_as_arrow examples
#'
#' @export
tri_from_arrow <- function(x) {
  check_nanoarrow()
  a <- nanoarrow::as_nanoarrow_array(x)
  sch <- nanoarrow::infer_nanoarrow_schema(a)
  meta <- sch$metadata[["ARROW:extension:metadata"]]
  fmt <- sch$format
  if (fmt %in% c("+l", "+L")) {
    off <- arrow_offsets(a)
    fsl <- a$children[[1]]
  } else if (startsWith(fmt, "+w:")) {
    off <- NULL
    fsl <- a
  } else {
    stop("`x` must be a fixed-size list of triangles or a list of them", call. = FALSE)
  }
  ## the indices of the triangles this array refers to
  if (is.null(off)) {
    rows <- fsl$offset + seq_len(fsl$length)
  } else {
    rows <- fsl$offset + seq.int(off[1] + 1L, length.out = off[length(off)] - off[1])
  }
  di <- fsl$children[[1]]
  if (is.null(di$dictionary)) {
    stop("the triangle corners must index a dictionary of vertices", call. = FALSE)
  }
  ix <- nanoarrow::convert_buffer(di$buffers[[2]])
  corner <- di$offset + rep((rows - 1L) * 3L, each = 3L) + 1:3
  code <- ix[corner] + 1L
  dict <- nanoarrow::convert_array(di$dictionary)
  ## one pool triangle per distinct index triple
  k <- matrix(code, nrow = 3L)
  key <- paste(k[1, ], k[2, ], k[3, ])
  uk <- !duplicated(key)
  tri_of_row <- match(key, key[uk])
  t <- k[, uk, drop = FALSE]
  v <- data.frame(.vx = seq_len(nrow(dict)), x = dict$x, y = dict$y)
  crs <- NULL
  if (!is.null(meta) && length(meta) && nzchar(meta)) {
    parsed <- tryCatch(arrow_meta_crs(meta), error = function(e) NULL)
    crs <- parsed
  }
  if (is.null(off)) {
    mesh <- new_wkpool_tri(v, t[1, ], t[2, ], t[3, ], crs = crs)
    return(list(mesh = mesh, group = NULL))
  }
  len <- diff(off)
  grp <- rep(seq_along(len), len)
  feature <- NULL
  if (!anyDuplicated(tri_of_row) && length(tri_of_row)) {
    feature <- integer(ncol(t))
    feature[tri_of_row] <- grp
  }
  mesh <- new_wkpool_tri(v, t[1, ], t[2, ], t[3, ], feature = feature, crs = crs)
  list(mesh = mesh, group = data.frame(triangle = tri_of_row, group = grp))
}

## ---- helpers ----

check_nanoarrow <- function() {
  if (!requireNamespace("nanoarrow", quietly = TRUE)) {
    stop("this needs the 'nanoarrow' package", call. = FALSE)
  }
}

tri_arrow_schema <- function(meta, feature) {
  coord <- nanoarrow::na_struct(list(x = nanoarrow::na_double(nullable = FALSE),
                                     y = nanoarrow::na_double(nullable = FALSE)),
                                nullable = FALSE)
  value <- nanoarrow::nanoarrow_schema_modify(
    nanoarrow::na_dictionary(coord, index_type = nanoarrow::na_int32()),
    list(flags = 0L))
  fsl <- nanoarrow::nanoarrow_schema_modify(
    nanoarrow::na_fixed_size_list(value, 3L, nullable = FALSE),
    list(children = list(vertices = value)))
  storage <- if (feature) {
    nanoarrow::nanoarrow_schema_modify(nanoarrow::na_list(fsl, nullable = FALSE),
                                       list(children = list(triangles = fsl)))
  } else {
    fsl
  }
  nanoarrow::na_extension(storage, "geoarrow.triangles", arrow_meta_json(meta))
}

tri_arrow_array <- function(vx, vy, code, offsets, meta) {
  verts <- nanoarrow::as_nanoarrow_array(data.frame(x = vx, y = vy))
  idx <- nanoarrow::as_nanoarrow_array(as.integer(code), schema = nanoarrow::na_int32())
  dict <- nanoarrow::nanoarrow_array_modify(idx, list(dictionary = verts), validate = FALSE)
  fsl <- nanoarrow::nanoarrow_array_init(nanoarrow::na_fixed_size_list(nanoarrow::na_int32(), 3L))
  fsl <- nanoarrow::nanoarrow_array_modify(
    fsl, list(length = length(code) %/% 3L, null_count = 0L, children = list(dict)),
    validate = FALSE)
  if (is.null(offsets)) {
    out <- fsl
  } else {
    out <- nanoarrow::nanoarrow_array_init(
      nanoarrow::na_list(nanoarrow::na_fixed_size_list(nanoarrow::na_int32(), 3L)))
    out <- nanoarrow::nanoarrow_array_modify(
      out, list(length = length(offsets) - 1L, null_count = 0L,
                buffers = list(NULL, nanoarrow::as_nanoarrow_buffer(as.integer(offsets))),
                children = list(fsl)),
      validate = FALSE)
  }
  nanoarrow::nanoarrow_array_set_schema(out, tri_arrow_schema(meta, !is.null(offsets)),
                                        validate = TRUE)
}

## list offsets of a list array, as integers, for its own rows
arrow_offsets <- function(a) {
  o <- nanoarrow::convert_buffer(a$buffers[[2]])
  as.integer(o[a$offset + seq_len(a$length + 1L)])
}

## minimal JSON for the extension metadata (strings only)
arrow_meta_json <- function(meta) {
  esc <- function(s) gsub("([\"\\\\])", "\\\\\\1", s)
  paste0("{", paste0("\"", names(meta), "\":\"", vapply(meta, esc, ""), "\"",
                     collapse = ","), "}")
}

arrow_meta_crs <- function(json) {
  m <- regmatches(json, regexec("\"crs\"\\s*:\\s*\"((?:[^\"\\\\]|\\\\.)*)\"", json, perl = TRUE))[[1]]
  if (length(m) < 2L) return(NULL)
  gsub("\\\\(.)", "\\1", m[2])
}
