## wkpool 0.5.0

This is an update to wkpool 0.4.0. It adds the `tri_*()` family for triangle
meshes (neighbours, measures, relate, overlay, dissolve, locate, Arrow
conversion), `find_faces()`, and compiled kernels for `tri_dissolve()` and
`find_faces()`. 'cdtr', 'geos' and 'nanoarrow' are new in Suggests.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Linux, R 4.6.1
* win-builder (devel)
* GitHub Actions - linux, windows, macos -  (see `.github/workflows`)
