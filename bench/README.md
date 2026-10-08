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
| unique undirected edges | `SC0()`, `SC()` | the above + `wkpool:::quotient_edges()` (internal) |
| arcs (TopoJSON-style) | `ARC()` | the above + `find_arcs(quotient = TRUE)` |

## Results (2026-10-08, Xeon 2.8 GHz, R 4.3.3, wkpool 0.3.0.9006 with vectorised verbs and positional vertex lookup)

Median seconds. wkpool always gets 3-10 iterations; silicate gets a single
iteration above 50k coords. `-` = not run: silicate `SC()`/`ARC()` are
skipped above 200k coords because they are superlinear (67 s and 39 s
already at 70k coords).

| input | PATH0 | wkpool vertices | SC0 | SC | wkpool edges | ARC | wkpool arcs |
|---|---|---|---|---|---|---|---|
| nc | 0.027 | 0.0039 | 0.054 | 0.11 | 0.0026 | 0.32 | 0.0038 |
| inlandwaters | 0.061 | 0.011 | 0.12 | 0.53 | 0.014 | 1.5 | 0.047 |
| hex_1e3 | 0.13 | 0.0045 | 0.25 | 0.79 | 0.0054 | 2.6 | 0.0064 |
| hex_1e4 | 0.88 | 0.06 | 1.7 | 71 | 0.056 | 33 | 0.061 |
| dense_hex_1e3 | 0.15 | 0.018 | 0.25 | 1.4 | 0.026 | 5 | 0.073 |
| lines_100x1000 | 0.053 | 0.028 | 0.13 | 0.88 | 0.076 | 16 | 0.082 |
| hex_1e5 | 12 | 0.48 | 20 | - | 0.52 | - | 0.67 |
| dense_hex_1e4 | 0.99 | 0.36 | 1.7 | - | 0.42 | - | 0.33 |
| lines_1000x1000 | 0.79 | 0.48 | 0.56 | - | 0.64 | - | 0.64 |

### Reading

* wkpool is faster than silicate on every input for vertices: 1.6-29x
  faster than `PATH0()`, from 1M coords of long lines (0.48 s vs 0.79 s)
  to 100k hexagons (0.48 s vs 12 s). For edges it is 1.7-46x faster than
  `SC0()` everywhere except 1M coords of long lines, where the two tie
  (0.64 s vs 0.56 s). Against `SC()` and `ARC()` it is 12-1300x faster.
* wkpool's pipelines scale linearly (exponent 0.7-0.8 over the scaling
  series, i.e. linear plus fixed cost); silicate `SC()` is superlinear
  (exponent 1.6) and `ARC()` allocates 4.2 GB on 10,000 hexagons.
* Before the positional vertex lookup (previous run), long lines were the
  one case silicate won: `PATH0()` was 1.6x faster at 1M coords. The cost
  was hash-table lookups of vertex ids: two `%in%` checks in
  `new_wkpool()` on every construction and a `match()` remap in
  `merge_coincident()`. Pools have `.vx = 1..n`, so the id is the
  position and none of that is needed. wkpool still allocates about 2x
  the memory of `PATH0()`.

### wkpool verbs

On the merged pool (median seconds):

| verb | nc | inlandwaters | hex_1e3 | hex_1e4 | dense_hex_1e3 | lines_100x1000 | hex_1e5 | dense_hex_1e4 | lines_1000x1000 |
|---|---|---|---|---|---|---|---|---|---|
| merge_coincident | 0.0023 | 0.013 | 0.0053 | 0.042 | 0.018 | 0.065 | 0.55 | 0.25 | 0.81 |
| pool_compact | 0.00079 | 0.011 | 0.00072 | 0.0092 | 0.017 | 0.017 | 0.24 | 0.13 | 0.22 |
| vertex_degree | 0.0017 | 0.0051 | 0.00027 | 0.0055 | 0.0097 | 0.014 | 0.13 | 0.053 | 0.22 |
| find_nodes | 0.0016 | 0.0063 | 0.00057 | 0.01 | 0.02 | 0.019 | 0.17 | 0.12 | 0.23 |
| find_arcs | 0.0023 | 0.0088 | 0.0014 | 0.014 | 0.028 | 0.029 | 0.24 | 0.15 | 0.41 |
| find_arcs_quotient | 0.0027 | 0.01 | 0.0013 | 0.013 | 0.02 | 0.039 | 0.18 | 0.1 | 0.28 |
| find_shared_edges | 0.0095 | 0.023 | 0.0088 | 0.13 | 0.13 | 0.058 | 1.1 | 1.4 | 2.1 |
| find_internal_boundaries | 0.00084 | 0.0024 | 0.0012 | 0.0054 | 0.0063 | 0.0083 | 0.12 | 0.13 | 0.18 |
| topology_report | 0.00092 | 0.0084 | 0.0013 | 0.0072 | 0.012 | 0.013 | 0.15 | 0.13 | 0.22 |
| find_cycles | 0.0012 | 0.0046 | 0.0088 | 0.12 | 0.02 | 0.008 | 1.1 | 0.18 | 0.14 |
| classify_cycles | 0.0026 | 0.0088 | 0.016 | 0.18 | 0.036 | 0.0089 | 2.1 | 0.36 | 0.14 |
| find_neighbours_edge | 0.0016 | 0.0059 | 0.0027 | 0.03 | 0.036 | 0.026 | 0.47 | 0.48 | 0.6 |
| find_neighbours_vertex | 0.0015 | 0.011 | 0.0046 | 0.054 | 0.12 | 0.022 | 0.74 | 0.41 | 1.2 |
| hole_points | 0.0016 | 0.0057 | 0.019 | 0.1 | 0.019 | 0.0086 | 1.6 | 0.21 | 0.15 |

All verbs scale linearly over the scaling series (exponents 0.45-1.21 in
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
(quadratic in features). `establish_topology()` is still 3-7x the cost
of `wk::wk_coords()`, which is the floor for this design; most of the
rest is building the vertex data frame and the segment record in R.
