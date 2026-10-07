# silicate vs wkpool: decomposition and normalization

`silicate-vs-wkpool.R` times silicate (CRAN 0.7.1) against wkpool on the same
`sf` inputs, from `nc.shp` up to a million coordinates, and checks that both
packages agree on the vertex and edge counts first. Results are in `results/`.

Rerun from the package root (wkpool, silicate, sf, wk, bench installed):

    Rscript bench/silicate-vs-wkpool.R          # all sizes, ~25 min
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

## Results (2026-10-07, Xeon 2.8 GHz, R 4.3.3, wkpool 0.3.0.9006 with the vectorised verbs)

Median seconds. wkpool always gets 3-10 iterations; silicate gets a single
iteration above 50k coords. `-` = not run: silicate `SC()`/`ARC()` are
skipped above 200k coords because they are superlinear (67 s and 39 s
already at 70k coords).

| input | PATH0 | wkpool vertices | SC0 | SC | wkpool edges | ARC | wkpool arcs |
|---|---|---|---|---|---|---|---|
| nc | 0.025 | 0.0022 | 0.051 | 0.083 | 0.0026 | 0.31 | 0.0038 |
| inlandwaters | 0.046 | 0.043 | 0.076 | 0.44 | 0.035 | 1.4 | 0.039 |
| hex_1e3 | 0.091 | 0.0054 | 0.19 | 0.67 | 0.0057 | 3 | 0.009 |
| hex_1e4 | 1.3 | 0.082 | 2.1 | 67 | 0.093 | 39 | 0.11 |
| dense_hex_1e3 | 0.15 | 0.092 | 0.31 | 1.3 | 0.12 | 4.7 | 0.12 |
| lines_100x1000 | 0.049 | 0.14 | 0.13 | 0.92 | 0.17 | 15 | 0.22 |
| hex_1e5 | 13 | 1.7 | 21 | - | 1.1 | - | 1.1 |
| dense_hex_1e4 | 1.2 | 0.57 | 2 | - | 0.53 | - | 0.8 |
| lines_1000x1000 | 0.46 | 0.76 | 0.58 | - | 0.85 | - | 1.1 |

### Reading

* Polygon coverages with many features: wkpool is 8-17x faster than
  `PATH0()` for vertices, 19-33x faster than `SC0()` for edges, 30-720x
  faster than `SC()`, and 80-360x faster than `ARC()`. wkpool's pipelines
  scale linearly (exponent 0.8-0.95 over the scaling series, i.e. linear
  plus fixed cost); silicate `SC()` is superlinear (exponent 1.6) and
  `ARC()` allocates 4.2 GB on 10,000 hexagons.
* Few features with long rings or lines (inlandwaters, dense hexagons,
  random-walk lines): vertices range from a tie to `PATH0()` being 3x
  faster on 100 lines of 1000 vertices; at 1M coords `PATH0()` is 1.6x
  faster and `SC0()` 1.5x faster. silicate's per-feature overhead is
  small when there are few features, while wkpool pays a per-coordinate
  constant: `establish_topology()` is ~5x `wk_coords()` on its own, then
  `merge_coincident()` adds about as much again. wkpool also allocates
  about 2x the memory of `PATH0()`. wkpool still wins against `SC()` and
  `ARC()` by 5-70x on these inputs.

### wkpool verbs

On the merged pool (median seconds):

| verb | nc | inlandwaters | hex_1e3 | hex_1e4 | dense_hex_1e3 | lines_100x1000 | hex_1e5 | dense_hex_1e4 | lines_1000x1000 |
|---|---|---|---|---|---|---|---|---|---|
| merge_coincident | 0.0035 | 0.025 | 0.007 | 0.095 | 0.11 | 0.16 | 0.78 | 0.58 | 0.71 |
| pool_compact | 0.00077 | 0.015 | 0.00074 | 0.012 | 0.025 | 0.023 | 0.42 | 0.21 | 0.32 |
| vertex_degree | 0.00013 | 0.005 | 0.0003 | 0.0058 | 0.0099 | 0.014 | 0.16 | 0.092 | 0.22 |
| find_nodes | 0.00032 | 0.0055 | 0.00059 | 0.016 | 0.02 | 0.018 | 0.18 | 0.19 | 0.23 |
| find_arcs | 0.00073 | 0.0082 | 0.0027 | 0.015 | 0.029 | 0.034 | 0.24 | 0.28 | 0.46 |
| find_arcs_quotient | 0.00074 | 0.01 | 0.0013 | 0.013 | 0.019 | 0.051 | 0.18 | 0.16 | 0.32 |
| find_shared_edges | 0.0031 | 0.011 | 0.0065 | 0.093 | 0.15 | 0.091 | 1.2 | 1.4 | 1.6 |
| find_internal_boundaries | 0.00088 | 0.0046 | 0.0012 | 0.0088 | 0.014 | 0.015 | 0.32 | 0.21 | 0.29 |
| topology_report | 0.0012 | 0.01 | 0.0019 | 0.011 | 0.019 | 0.021 | 0.28 | 0.25 | 0.34 |
| find_cycles | 0.0012 | 0.0045 | 0.012 | 0.14 | 0.018 | 0.0096 | 1.1 | 0.41 | 0.12 |
| classify_cycles | 0.0025 | 0.011 | 0.019 | 0.23 | 0.033 | 0.011 | 3.2 | 0.45 | 0.13 |
| find_neighbours_edge | 0.0018 | 0.013 | 0.0033 | 0.03 | 0.034 | 0.037 | 0.39 | 0.47 | 0.68 |
| find_neighbours_vertex | 0.0016 | 0.0093 | 0.0075 | 0.053 | 0.063 | 0.023 | 0.83 | 0.47 | 1.3 |
| hole_points | 0.0016 | 0.007 | 0.012 | 0.11 | 0.022 | 0.01 | 1.5 | 0.21 | 0.17 |

All verbs scale linearly over the scaling series (exponents 0.6-1.26 in
`results/scaling-exponents.csv`).

Before the vectorisation (same machine, hex_1e4), the quadratic verbs were:

| verb | before | after |
|---|---|---|
| find_cycles | 2.0 s (4.7 GB) | 0.14 s |
| classify_cycles | 8.3 s (7.9 GB) | 0.23 s |
| hole_points | 2.1 s | 0.11 s |
| find_neighbours(type = "edge") | 19 s (13 GB) | 0.03 s |
| find_neighbours(type = "vertex") | 2.9 s | 0.05 s |

They were skipped above 100k coords in the earlier runs. The causes were
`which(path == p)` per path in `find_cycles()`, one pool-wide `match()`
per ring for areas and centroids, and a per-key scan with `expand.grid()`
in `find_neighbours()`.

Remaining: `cycles_to_wkb()` loops over features with `feat == f`
(quadratic in features), and the core pipeline's per-coordinate constant
is what loses to `PATH0()` on long lines.
