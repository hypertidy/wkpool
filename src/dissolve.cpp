// Dissolve kernel for tri_dissolve()
//
// Mirrors the R reference implementation (dissolve_rings_r() in
// R/tri_dissolve.R) step for step, so the two return identical rings in
// identical order; the tests compare them.
//
// Directed edge h (0-based) is edge k of triangle i with h = k * nt + i,
// running from corner k to corner k + 1. `after[h]` is the next edge round
// the same triangle. An edge is inner when its twin (the reversed edge)
// exists and is in the same group. A boundary edge is followed by turning
// through the fan of the group's triangles around the vertex it ends at,
// so the walk needs no angles. Rings that visit a vertex twice are split
// there. Each hole goes to the outer ring of its edge-connected component;
// only when a component has several outer rings is a point-in-ring test
// used (the centroid of the triangle on the hole's inner side).
//
// Input rows t0, t1, t2 are 1-based into vx, vy; group is 1-based with no
// NA (the caller drops NA triangles). Output: a list of ring vertices (not
// closed) in output order with x, y, ring, poly (1, 2, ... in output order)
// and group.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <unordered_map>
#include <vector>

using namespace cpp11;

namespace {

int find_root(std::vector<int>& parent, int a) {
  while (parent[a] != a) {
    parent[a] = parent[parent[a]];
    a = parent[a];
  }
  return a;
}

// even-odd crossing test of a point against one ring (not closed), as
// point_in_ring() in R
bool point_in_ring(double px, double py, const double* x, const double* y, int n) {
  int count = 0;
  for (int i = 0; i < n; i++) {
    int j = (i == 0) ? n - 1 : i - 1;
    bool up = (y[i] > py) != (y[j] > py);
    if (!up) continue;
    double xc = x[i] + (py - y[i]) * (x[j] - x[i]) / (y[j] - y[i]);
    if (px < xc) count++;
  }
  return (count % 2) == 1;
}

}  // namespace

[[cpp11::register]]
cpp11::writable::list dissolve_rings_cpp(cpp11::integers t0, cpp11::integers t1,
                                         cpp11::integers t2, cpp11::integers group,
                                         cpp11::doubles vx, cpp11::doubles vy) {
  const int nt = t0.size();
  const int ne = 3 * nt;
  const long long nv = vx.size();

  std::vector<int> e0(ne), e1(ne), et(ne), after(ne);
  for (int i = 0; i < nt; i++) {
    int a = t0[i] - 1, b = t1[i] - 1, c = t2[i] - 1;
    e0[i] = a;          e1[i] = b;
    e0[nt + i] = b;     e1[nt + i] = c;
    e0[2 * nt + i] = c; e1[2 * nt + i] = a;
    et[i] = et[nt + i] = et[2 * nt + i] = i;
    after[i] = nt + i;
    after[nt + i] = 2 * nt + i;
    after[2 * nt + i] = i;
  }

  // twin: the first directed edge running the other way (R's match())
  std::unordered_map<long long, int> first_edge;
  first_edge.reserve(ne);
  for (int h = 0; h < ne; h++) {
    long long key = (long long) e0[h] * nv + e1[h];
    first_edge.emplace(key, h);
  }
  std::vector<int> twin(ne, -1);
  std::vector<char> inner(ne, 0);
  for (int h = 0; h < ne; h++) {
    auto it = first_edge.find((long long) e1[h] * nv + e0[h]);
    if (it != first_edge.end()) {
      twin[h] = it->second;
      inner[h] = group[et[h]] == group[et[twin[h]]];
    }
  }

  // edge-connected components of triangles
  std::vector<int> parent(nt);
  std::iota(parent.begin(), parent.end(), 0);
  for (int h = 0; h < ne; h++) {
    if (!inner[h]) continue;
    int a = find_root(parent, et[h]), b = find_root(parent, et[twin[h]]);
    if (a != b) parent[std::max(a, b)] = std::min(a, b);
  }
  std::vector<int> comp(nt);
  for (int t = 0; t < nt; t++) comp[t] = find_root(parent, t);

  // boundary edges, and the boundary edge that follows each
  std::vector<int> b;
  for (int h = 0; h < ne; h++) if (!inner[h]) b.push_back(h);
  const int nb = b.size();
  std::vector<int> pos_in_b(ne, -1);
  for (int k = 0; k < nb; k++) pos_in_b[b[k]] = k;
  std::vector<int> nxt(nb, -1);
  for (int k = 0; k < nb; k++) {
    int j = after[b[k]];
    int guard = 0;
    while (inner[j] && guard <= nt) {
      j = after[twin[j]];
      guard++;
    }
    nxt[k] = pos_in_b[j];
  }

  // walk the cycles: ring id and position for each boundary edge
  std::vector<int> ring(nb, 0), pos(nb, 0);
  int r = 0;
  for (int s = 0; s < nb; s++) {
    if (ring[s] > 0) continue;
    r++;
    int k = 0;
    int e = s;
    while (e >= 0 && ring[e] == 0) {
      k++;
      ring[e] = r;
      pos[e] = k;
      e = nxt[e];
    }
  }
  std::vector<int> o(nb);
  std::iota(o.begin(), o.end(), 0);
  std::stable_sort(o.begin(), o.end(), [&](int p, int q) {
    return ring[p] < ring[q] || (ring[p] == ring[q] && pos[p] < pos[q]);
  });
  std::vector<int> wr(nb), wv(nb);   // ring and start vertex in walk order
  for (int k = 0; k < nb; k++) {
    wr[k] = ring[o[k]];
    wv[k] = e0[b[o[k]]];
  }

  // split rings that visit a vertex more than once (split_rings() in R)
  int next_id = r;
  {
    int k = 0;
    while (k < nb) {
      int kend = k;
      while (kend < nb && wr[kend] == wr[k]) kend++;
      std::vector<int> idx;
      for (int i = k; i < kend; i++) idx.push_back(i);
      for (;;) {
        // first position whose vertex appeared earlier in idx
        std::unordered_map<int, int> seen;
        int jpos = -1, ipos = -1;
        for (int p = 0; p < (int) idx.size(); p++) {
          auto it = seen.find(wv[idx[p]]);
          if (it != seen.end()) {
            jpos = p;
            ipos = it->second;
            break;
          }
          seen.emplace(wv[idx[p]], p);
        }
        if (jpos < 0) break;
        next_id++;
        for (int p = ipos; p < jpos; p++) wr[idx[p]] = next_id;
        idx.erase(idx.begin() + ipos, idx.begin() + jpos);
      }
      k = kend;
    }
  }
  // regroup by ring id, keeping walk order within each ring
  std::vector<int> o2(nb);
  std::iota(o2.begin(), o2.end(), 0);
  std::stable_sort(o2.begin(), o2.end(), [&](int p, int q) { return wr[p] < wr[q]; });
  const int nr = next_id;
  std::vector<int> rr(nb), vxr(nb), owner(nb);
  std::vector<double> xr(nb), yr(nb);
  for (int k = 0; k < nb; k++) {
    int w = o2[k];
    rr[k] = wr[w];
    vxr[k] = wv[w];
    owner[k] = et[b[o[w]]];
    xr[k] = vx[vxr[k]];
    yr[k] = vy[vxr[k]];
  }

  // ring extents in rr (contiguous, ids 1..nr)
  std::vector<int> rstart(nr + 2, -1), rlen(nr + 2, 0);
  for (int k = 0; k < nb; k++) {
    if (rstart[rr[k]] < 0) rstart[rr[k]] = k;
    rlen[rr[k]]++;
  }

  // signed area per ring, translated to the ring's first vertex
  std::vector<double> area(nr + 1, 0.0);
  for (int id = 1; id <= nr; id++) {
    int s = rstart[id], n = rlen[id];
    if (n == 0) continue;
    double x0 = xr[s], y0 = yr[s];
    double sum = 0.0;
    for (int p = 0; p < n; p++) {
      int a = s + p, c = s + (p + 1) % n;
      double xs = xr[a] - x0, ys = yr[a] - y0;
      double xn = xr[c] - x0, yn = yr[c] - y0;
      sum += (xs * yn - xn * ys) / 2;
    }
    area[id] = sum;
  }
  std::vector<int> ring_comp(nr + 1), ring_group(nr + 1), ring_owner(nr + 1);
  std::vector<char> is_outer(nr + 1);
  for (int id = 1; id <= nr; id++) {
    int s = rstart[id];
    ring_owner[id] = owner[s];
    ring_comp[id] = comp[owner[s]];
    ring_group[id] = group[owner[s]];
    is_outer[id] = area[id] > 0;
  }

  // polygon (outer ring id) of each ring
  std::vector<int> poly(nr + 1, 0);
  std::unordered_map<int, std::vector<int>> outer_by_comp;
  for (int id = 1; id <= nr; id++) {
    if (is_outer[id]) {
      poly[id] = id;
      outer_by_comp[ring_comp[id]].push_back(id);
    }
  }
  for (int h = 1; h <= nr; h++) {
    if (is_outer[h]) continue;
    auto it = outer_by_comp.find(ring_comp[h]);
    if (it == outer_by_comp.end() || it->second.empty()) {
      poly[h] = h;   // no outer ring in the component (degenerate input)
      continue;
    }
    const std::vector<int>& cand_all = it->second;
    if (cand_all.size() == 1) {
      poly[h] = cand_all[0];
      continue;
    }
    int t = ring_owner[h];
    int a = t0[t] - 1, bb = t1[t] - 1, c = t2[t] - 1;
    double px = (vx[a] + vx[bb] + vx[c]) / 3;
    double py = (vy[a] + vy[bb] + vy[c]) / 3;
    std::vector<int> cand;
    for (int id : cand_all) {
      if (point_in_ring(px, py, &xr[rstart[id]], &yr[rstart[id]], rlen[id])) cand.push_back(id);
    }
    if (cand.empty()) cand = cand_all;
    int best = cand[0];
    for (int id : cand) if (std::fabs(area[id]) < std::fabs(area[best])) best = id;
    poly[h] = best;
  }

  // rings in output order: by group, then polygon, outer ring first
  std::vector<int> ids(nr);
  std::iota(ids.begin(), ids.end(), 1);
  std::stable_sort(ids.begin(), ids.end(), [&](int p, int q) {
    int gp = ring_group[poly[p]], gq = ring_group[poly[q]];
    if (gp != gq) return gp < gq;
    if (poly[p] != poly[q]) return poly[p] < poly[q];
    return (!is_outer[p]) < (!is_outer[q]);
  });
  std::vector<int> rank(nr + 1);
  for (int k = 0; k < nr; k++) rank[ids[k]] = k + 1;

  writable::doubles ox(nb), oy(nb);
  writable::integers oring(nb), opoly(nb), ogroup(nb);
  std::unordered_map<int, int> poly_rank;
  int row = 0;
  for (int k = 0; k < nr; k++) {
    int id = ids[k];
    int s = rstart[id], n = rlen[id];
    int pid = poly[id];
    auto pit = poly_rank.find(pid);
    int pr;
    if (pit == poly_rank.end()) {
      pr = (int) poly_rank.size() + 1;
      poly_rank.emplace(pid, pr);
    } else {
      pr = pit->second;
    }
    for (int p = 0; p < n; p++) {
      ox[row] = xr[s + p];
      oy[row] = yr[s + p];
      oring[row] = k + 1;
      opoly[row] = pr;
      ogroup[row] = ring_group[id];
      row++;
    }
  }
  using namespace cpp11::literals;
  return writable::list({"x"_nm = ox, "y"_nm = oy, "ring"_nm = oring,
                         "poly"_nm = opoly, "group"_nm = ogroup});
}
