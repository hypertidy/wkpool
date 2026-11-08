// Face enumeration kernel
//
// Mirrors the R reference implementation (find_faces_walk_r) exactly.
//
// Each undirected edge e (0-based, dense vertex positions i0[e], i1[e])
// is two half-edges: 2e runs i0 -> i1, 2e + 1 runs i1 -> i0, and the
// twin of h is h ^ 1. At every vertex the outgoing half-edges are
// ordered counter-clockwise by angle (ties by half-edge id). The next
// half-edge after h, arriving at v, is the one clockwise-adjacent to
// twin(h) in v's order, so every walk keeps its face on the left:
// bounded faces come out counter-clockwise and the outer face of each
// connected component clockwise. `next` is a permutation, so the walks
// partition the half-edges: each lies on exactly one face.
//
// Output: `order`, all half-edges (0-based) in walk order, face by
// face, each face starting at its lowest unused half-edge; `face`, the
// 0-based face of each half-edge, indexed by half-edge.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace cpp11;

[[cpp11::register]]
cpp11::list find_faces_cpp(cpp11::integers i0, cpp11::integers i1,
                           cpp11::doubles x, cpp11::doubles y) {
  const int n = i0.size();
  const int n_vertices = x.size();
  const int nh = 2 * n;

  auto from = [&](int h) { return (h & 1) ? i1[h >> 1] : i0[h >> 1]; };
  auto to = [&](int h) { return (h & 1) ? i0[h >> 1] : i1[h >> 1]; };

  // outgoing half-edges per vertex (CSR), then sort each slice by angle
  std::vector<int> deg(n_vertices, 0);
  for (int h = 0; h < nh; h++) deg[from(h)]++;
  std::vector<int> offset(n_vertices + 1, 0);
  for (int v = 0; v < n_vertices; v++) offset[v + 1] = offset[v] + deg[v];
  std::vector<int> rot(nh);
  std::vector<int> fill(offset.begin(), offset.end() - 1);
  for (int h = 0; h < nh; h++) rot[fill[from(h)]++] = h;

  std::vector<double> angle(nh);
  for (int h = 0; h < nh; h++) {
    const int a = from(h), b = to(h);
    angle[h] = std::atan2(y[b] - y[a], x[b] - x[a]);
  }
  std::vector<int> pos(nh);
  for (int v = 0; v < n_vertices; v++) {
    std::sort(rot.begin() + offset[v], rot.begin() + offset[v + 1],
              [&](int p, int q) {
                if (angle[p] != angle[q]) return angle[p] < angle[q];
                return p < q;
              });
    for (int k = offset[v]; k < offset[v + 1]; k++) pos[rot[k]] = k - offset[v];
  }

  auto next = [&](int h) {
    const int v = to(h);
    const int t = h ^ 1;
    const int d = offset[v + 1] - offset[v];
    return rot[offset[v] + (pos[t] - 1 + d) % d];
  };

  writable::integers order(nh);
  writable::integers face(nh);
  std::vector<bool> used(nh, false);
  int k = 0, f = 0;
  for (int start = 0; start < nh; start++) {
    if (used[start]) continue;
    int h = start;
    do {
      used[h] = true;
      face[h] = f;
      order[k++] = h;
      h = next(h);
    } while (h != start);
    f++;
  }

  writable::list out(2);
  out[0] = order;
  out[1] = face;
  out.names() = {"order", "face"};
  return out;
}
