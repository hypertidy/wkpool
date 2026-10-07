# silicate vs wkpool: decomposition and normalization

`silicate-vs-wkpool.R` times silicate (CRAN 0.7.1) against wkpool on the same
`sf` inputs, from `nc.shp` up to a million coordinates, and checks that both
packages agree on the vertex and edge counts first. Results are in `results/`.

Rerun from the package root (wkpool, silicate, sf, wk, bench installed):

    Rscript bench/silicate-vs-wkpool.R          # all sizes, ~20 min
    Rscript bench/silicate-vs-wkpool.R small    # nc + 1000 hexagons, ~2 min

| file | contents |
|---|---|
| `results/timings.csv` | median/min seconds, MB allocated, iterations, per input x task x expression |
| `results/counts.csv` | coordinate, vertex and unique-edge counts from each package (they all agree) |
| `results/scaling.csv` | hexagon coverages of 250..8000 cells, every pipeline and verb |
| `results/scaling-exponents.csv` | log-log slope of time vs coordinates (1 = linear, 2 = quadratic) |
| `results/session.txt` | package versions, CPU, sessionInfo |

## Inputs

| input | features | coords | unique vertices | unique edges |
|---|---|---|---|---|
| nc | 100 | 2,529 | 1,255 | 1,357 |
| inlandwaters (silicate data; 189 rings, 32 holes) | 6 | 33,644 | 30,835 | 30,843 |
| hex_1e3 | 1,000 | 7,000 | 2,128 | 3,127 |
| hex_1e4 | 10,000 | 70,000 | 20,404 | 30,403 |
| dense_hex_1e3 (edges densified x10) | 1,000 | 73,000 | 37,818 | 39,510 |
| lines_100x1000 (random walks on a lattice) | 100 | 100,000 | 28,090 | 61,854 |
| hex_1e5 | 100,000 | 700,000 | 201,280 | 301,279 |
| dense_hex_1e4 | 10,000 | 730,000 | 359,008 | 371,257 |
| lines_1000x1000 | 1,000 | 1,000,000 | 62,418 | 249,364 |

## Pairings

| task | silicate | wkpool |
|---|---|---|
| vertices (exact identity) | `PATH0()`, `PATH()` | `merge_coincident(establish_topology(x))` |
| unique undirected edges | `SC0()`, `SC()` | the above + `quotient_edges()` |
| arcs (TopoJSON-style) | `ARC()` | the above + `find_arcs(quotient = TRUE)` |

## Results (2026-10-07 second run, Xeon 2.8 GHz, R 4.3.3, wkpool 0.3.0.9006 @ 4cc1fa7)

Median seconds; single iterations above 50k coords. `-` = not run: silicate
`SC()`/`ARC()` are skipped above 200k coords because they are superlinear
(49 s and 34 s already at 70k coords).

| input | PATH0 | wkpool vertices | SC0 | SC | wkpool edges | ARC | wkpool arcs |
|---|---|---|---|---|---|---|---|
| nc | 0.029 | 0.003 | 0.055 | 0.084 | 0.0029 | 0.32 | 0.0064 |
| inlandwaters | 0.049 | 0.045 | 0.071 | 0.47 | 0.039 | 1.4 | 0.059 |
| hex_1e3 | 0.13 | 0.0062 | 0.26 | 0.82 | 0.0047 | 2.6 | 0.0068 |
| hex_1e4 | 0.91 | 0.063 | 2.1 | 49 | 0.067 | 34 | 0.086 |
| dense_hex_1e3 | 0.18 | 0.065 | 0.4 | 1.6 | 0.069 | 5.3 | 0.14 |
| lines_100x1000 | 0.055 | 0.1 | 0.24 | 0.69 | 0.17 | 18 | 0.13 |
| hex_1e5 | 11 | 1.5 | 21 | - | 1.1 | - | 2 |
| dense_hex_1e4 | 1.4 | 1.4 | 2 | - | 0.74 | - | 0.7 |
| lines_1000x1000 | 0.46 | 2.1 | 0.81 | - | 0.99 | - | 1.6 |

Single-iteration timings (above 50k coords) vary by up to 2x between runs
because of garbage collection: an earlier run had wkpool vertices at 0.91 s
for dense_hex_1e4 and 1.1 s for lines_1000x1000. Ratios below 2x are noise.

### Reading

* Polygon coverages with many features: wkpool is 3-20x faster than
  `PATH0()` for vertices, 3-55x faster than `SC0()` for edges, 23-730x
  faster than `SC()`, and 37-390x faster than `ARC()`. wkpool's pipelines
  scale linearly (exponent ~0.8-0.9 over the scaling series, i.e. linear plus
  fixed cost); silicate `SC()` is superlinear (exponent 1.7; 60x slower for
  10x more hexagons) and `ARC()` allocates 4.2 GB on 10,000 hexagons.
* inlandwaters (few features, long rings, holes): vertices tie with `PATH0()`,
  edges 1.8x faster than `SC0()`, 12x faster than `SC()`, 23x faster than
  `ARC()`.
* Long lines with few features: silicate's integer-indexed `PATH0()` is
  2-5x faster than wkpool (1M coords: 0.46 s vs 1.1-2.1 s) and `SC0()` ties.
  silicate's per-feature overhead is small when there are few features,
  while wkpool pays roughly 1-2 us per coordinate: `establish_topology()`
  is ~5x `wk_coords()` on its own, then `merge_coincident()` adds about as
  much again. wkpool also allocates about 2x the memory of `PATH0()`.

### wkpool verbs that need attention

On the merged pool (seconds; skipped above 100k coords where marked `-`):

| verb | inlandwaters | hex_1e3 | hex_1e4 | hex_1e5 | exponent |
|---|---|---|---|---|---|
| find_cycles | 0.038 | 0.050 | 2.6 (4.7 GB) | - | 2.0 |
| classify_cycles | 0.30 | 0.11 | 9.5 (7.9 GB) | - | 2.0 |
| find_neighbours(type = "edge") | 0.78 | 0.72 | 20 (13 GB) | - | 1.6 |
| find_neighbours(type = "vertex") | 0.58 | 0.40 | 3.3 | - | 1.1 |
| find_shared_edges | 0.26 | 0.021 | 0.30 | 3.2 | 1.1 |
| find_internal_boundaries | 0.064 | 0.0085 | 0.098 | 1.4 | 1.2 |
| topology_report | 0.18 | 0.015 | 0.24 | 2.4 | 1.1 |

For comparison the healthy verbs at hex_1e5: `vertex_degree` 0.18 s,
`find_arcs` 0.28 s, `pool_compact` 0.39 s, `merge_coincident` 0.97 s.

Causes, from the code:

* `find_cycles()` loops over paths with `which(path == p)`: O(paths x
  segments). `split(seq_along(path), path)` makes it linear.
  `classify_cycles()` and `hole_points()` inherit this, and add their own
  per-cycle `match(cycle, pool$.vx)` over the whole pool: O(cycles x
  vertices), which is why `classify_cycles()` is 8x `find_cycles()` on
  inlandwaters (189 rings, 31k vertices).
* `find_neighbours(type = "edge")` loops over shared edge keys with
  `shared$edge_key == key` and `expand.grid()` per key: O(keys x rows).
  It also goes through `find_shared_edges()`, which builds `paste()` keys and
  a `tapply()` with a closure. Grouping with `vctrs::vec_group_id()` (as
  `quotient_edges()` already does) and a self-join on the edge group would
  be linear.
* `find_neighbours(type = "vertex")` builds one `expand.grid()` per shared
  vertex.
* `find_internal_boundaries()` uses `paste()` keys and `%in%`; integer keys
  (`vx0 * n + vx1`, or `vec_group_id`) would avoid string allocation.
