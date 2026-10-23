# wkpool: Vertex Pool Topology for Well-Known Geometry

Establishes and maintains vertex pool topology for geometry handled by
'wk'. Segments are the atomic unit, vertices are shared via integer
references into a pool. Topology is made discoverable via coincident
vertex detection while not requiring modification of the input data.
Topological data models follow principles described in Worboys and
Duckham (2004, ISBN:978-0415283755). The edge-based topology geometry
decomposed into vertices and directed edge pairs is a simplification of
the quad-edge case in Guibas & Stolfi (1985)
[doi:10.1145/282918.282923](https://doi.org/10.1145/282918.282923) .

## See also

Useful links:

- <https://github.com/hypertidy/wkpool>

- Report bugs at <https://github.com/hypertidy/wkpool/issues>

## Author

**Maintainer**: Michael Sumner <mdsumner@gmail.com> \[copyright holder\]

Authors:

- Michael Sumner <mdsumner@gmail.com> \[copyright holder\]
