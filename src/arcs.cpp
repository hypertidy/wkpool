// Arc extraction kernel
//
// Mirrors the R reference implementation (find_arcs_walk_r) exactly:
// same adjacency ordering (segments ascending, start endpoint before
// end endpoint within a segment), same node ordering (ascending dense
// vertex position, i.e. ascending .vx id), same walk semantics
// (amortized per-vertex cursor over incident segments), same output
// (arcs from nodes first, then leftover closed loops). The only
// difference is the constant factor.
//
// Inputs are dense 0-based vertex positions plus the sorted id
// vector; output arcs carry the original .vx ids directly.

#include <cpp11.hpp>
#include <vector>

using namespace cpp11;

[[cpp11::register]]
cpp11::list find_arcs_cpp(cpp11::integers i0, cpp11::integers i1,
                          cpp11::integers ids) {
  const int n = i0.size();
  const int n_vertices = ids.size();

  // CSR adjacency in the reference order: for each segment s ascending,
  // record s at its start vertex, then at its end vertex
  std::vector<int> deg(n_vertices, 0);
  for (int s = 0; s < n; s++) {
    deg[i0[s]]++;
    deg[i1[s]]++;
  }
  std::vector<int> offset(n_vertices + 1, 0);
  for (int v = 0; v < n_vertices; v++) offset[v + 1] = offset[v] + deg[v];
  std::vector<int> adj(2 * (size_t)n);
  std::vector<int> fill(offset.begin(), offset.end() - 1);
  for (int s = 0; s < n; s++) {
    adj[fill[i0[s]]++] = s;
    adj[fill[i1[s]]++] = s;
  }

  std::vector<bool> is_node(n_vertices);
  for (int v = 0; v < n_vertices; v++) is_node[v] = (deg[v] != 2);

  // amortized cursor per vertex into its adjacency slice
  std::vector<int> cursor(offset.begin(), offset.end() - 1);
  std::vector<bool> used(n, false);

  // arcs collected as one buffer of vertex positions plus lengths
  std::vector<int> verts;
  verts.reserve((size_t)n + 16);
  std::vector<int> lens;

  std::vector<int> buf((size_t)n + 1);

  auto next_unused = [&](int v) -> int {
    int k = cursor[v];
    const int end = offset[v + 1];
    while (k < end && used[adj[k]]) k++;
    cursor[v] = k;
    return (k < end) ? adj[k] : -1;
  };

  auto walk = [&](int start_pos, int start_seg, bool stop_at_node) {
    int k = 0;
    buf[k++] = start_pos;
    int current = start_pos;
    int seg = start_seg;

    for (;;) {
      used[seg] = true;
      int nxt = (i0[seg] == current) ? i1[seg] : i0[seg];
      buf[k++] = nxt;

      if (stop_at_node && is_node[nxt]) break;
      if (!stop_at_node && nxt == start_pos) break;

      seg = next_unused(nxt);
      if (seg < 0) break;
      current = nxt;
    }

    verts.insert(verts.end(), buf.begin(), buf.begin() + k);
    lens.push_back(k);
  };

  // arcs from nodes, ascending vertex position
  for (int v = 0; v < n_vertices; v++) {
    if (!is_node[v]) continue;
    for (int k = offset[v]; k < offset[v + 1]; k++) {
      int s = adj[k];
      if (used[s]) continue;
      walk(v, s, true);
    }
  }

  // leftover closed loops (all degree-2)
  for (int s = 0; s < n; s++) {
    if (used[s]) continue;
    walk(i0[s], s, false);
  }

  // build the list of arc vectors, mapped back to original .vx ids
  const int n_arcs = (int)lens.size();
  writable::list out(n_arcs);
  size_t at = 0;
  for (int a = 0; a < n_arcs; a++) {
    writable::integers arc(lens[a]);
    for (int j = 0; j < lens[a]; j++) arc[j] = ids[verts[at++]];
    out[a] = arc;
  }
  return out;
}
