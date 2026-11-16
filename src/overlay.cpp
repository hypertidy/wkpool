// Kernels for tri_overlay() and tri_overlay_relate(): following input
// segments along the fixed edges of an overlay triangulation, and the
// DE-9IM matrix between groups of any dimension on one mesh.
//
// Conventions (set up by the R callers in R/tri_overlay.R):
// - t0, t1, t2: 1-based rows into the vertex table, counter-clockwise.
// - nbr: the edge table, 3 * nt values, position 3 * t + k (0-based t) is
//   the 1-based triangle across the edge from corner k to corner k + 1, or
//   0 at the hull.
// - a group has a dimension: 2 (a set of triangles), 1 (a set of mesh
//   edges, given by their two vertex rows) or 0 (a set of vertices). Its
//   members are g_item[g_start[g] - 1 .. g_start[g + 1] - 2] (1-based
//   rows), with g_item2 holding the second vertex of an edge.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <string>
#include <unordered_map>
#include <vector>

using namespace cpp11;

namespace {

inline long long edge_key(int u, int v, long long nv) {
  if (u > v) std::swap(u, v);
  return (long long) u * nv + v;
}

}  // namespace

// Follow each input segment (vertex rows a -> b of the overlay) along the
// fixed edges, from a to b: at each step take the fixed edge whose far end
// is nearest the line a-b among those that move on towards b. Returns the
// pieces as (segment, from, to), 1-based; a segment that cannot be followed
// gives a single row with from = to = 0.
[[cpp11::register]]
integers_matrix<> overlay_walk_cpp(doubles x, doubles y, integers s0, integers s1,
                                   integers a, integers b) {
  const int nv = x.size();
  const int ns = s0.size();
  std::vector<int> deg(nv + 1, 0), adj(2 * (size_t) ns);
  for (int i = 0; i < ns; i++) {
    deg[s0[i]]++;
    deg[s1[i]]++;
  }
  std::vector<int> start(nv + 1, 0);
  for (int v = 0; v < nv; v++) start[v + 1] = start[v] + deg[v + 1];
  {
    std::vector<int> fill(start.begin(), start.end() - 1);
    for (int i = 0; i < ns; i++) {
      adj[fill[s0[i] - 1]++] = s1[i] - 1;
      adj[fill[s1[i] - 1]++] = s0[i] - 1;
    }
  }
  std::vector<int> o_seg, o_from, o_to;
  const int n = a.size();
  for (int k = 0; k < n; k++) {
    const int va = a[k] - 1, vb = b[k] - 1;
    if (va == vb) continue;
    const double ax = x[va], ay = y[va];
    const double dx = x[vb] - ax, dy = y[vb] - ay;
    const double len2 = dx * dx + dy * dy;
    const size_t mark = o_seg.size();
    int cur = va;
    double tcur = 0.0;
    bool ok = true;
    int guard = 0;
    while (cur != vb) {
      int best = -1;
      double best_perp = 0.0, best_t = 0.0;
      for (int q = start[cur]; q < start[cur + 1]; q++) {
        const int w = adj[q];
        const double wx = x[w] - ax, wy = y[w] - ay;
        const double t = (wx * dx + wy * dy) / len2;
        if (!(t > tcur) || t > 1.0 + 1e-9) continue;
        const double perp = std::fabs(wx * dy - wy * dx) / std::sqrt(len2);
        if (best < 0 || perp < best_perp || (perp == best_perp && w == vb)) {
          best = w;
          best_perp = perp;
          best_t = t;
        }
      }
      if (best < 0 || ++guard > nv) {
        ok = false;
        break;
      }
      o_seg.push_back(k + 1);
      o_from.push_back(cur + 1);
      o_to.push_back(best + 1);
      cur = best;
      tcur = best_t;
    }
    if (!ok) {
      o_seg.resize(mark);
      o_from.resize(mark);
      o_to.resize(mark);
      o_seg.push_back(k + 1);
      o_from.push_back(0);
      o_to.push_back(0);
    }
  }
  const int m = o_seg.size();
  writable::integers_matrix<> out(m, 3);
  for (int i = 0; i < m; i++) {
    out(i, 0) = o_seg[i];
    out(i, 1) = o_from[i];
    out(i, 2) = o_to[i];
  }
  return out;
}

// Regions of the overlay: triangles joined across edges that are not
// fixed. No constraint of any layer runs between triangles of one region,
// so they all belong to the same polygon groups. Returns, for each
// triangle, the (1-based) triangle of its region with the largest area:
// the one whose centroid is safest to locate in a source mesh.
[[cpp11::register]]
integers overlay_regions_cpp(doubles x, doubles y, integers t0, integers t1,
                             integers t2, integers nbr, integers s0, integers s1) {
  const int nt = t0.size();
  const long long nv = x.size();
  std::unordered_map<long long, char> fixed;
  fixed.reserve(s0.size());
  for (int i = 0; i < (int) s0.size(); i++) fixed.emplace(edge_key(s0[i] - 1, s1[i] - 1, nv), 1);
  auto corner = [&](int t, int k) {
    return (k == 0 ? t0[t] : (k == 1 ? t1[t] : t2[t])) - 1;
  };
  std::vector<int> parent(nt);
  for (int t = 0; t < nt; t++) parent[t] = t;
  auto root = [&](int a) {
    while (parent[a] != a) {
      parent[a] = parent[parent[a]];
      a = parent[a];
    }
    return a;
  };
  for (int t = 0; t < nt; t++) {
    for (int k = 0; k < 3; k++) {
      const int u = nbr[3 * t + k] - 1;
      if (u < 0 || u < t) continue;
      if (fixed.count(edge_key(corner(t, k), corner(t, (k + 1) % 3), nv))) continue;
      const int a = root(t), b = root(u);
      if (a != b) parent[std::max(a, b)] = std::min(a, b);
    }
  }
  std::vector<double> area(nt);
  for (int t = 0; t < nt; t++) {
    const int a = corner(t, 0), b = corner(t, 1), c = corner(t, 2);
    area[t] = ((x[b] - x[a]) * (y[c] - y[a]) - (x[c] - x[a]) * (y[b] - y[a])) / 2;
  }
  std::vector<int> best(nt, -1);
  for (int t = 0; t < nt; t++) {
    const int r = root(t);
    if (best[r] < 0 || area[t] > area[best[r]]) best[r] = t;
  }
  writable::integers out(nt);
  for (int t = 0; t < nt; t++) out[t] = best[root(t)] + 1;
  return out;
}

// DE-9IM between groups of dimension 0, 1 or 2 on one triangle mesh.
//
// Every point of the plane is in exactly one open triangle, open edge or
// vertex of the mesh, or outside it. Each such cell is in the interior (I),
// boundary (B) or exterior (E) of a group:
// - dimension 2: a triangle is I when it is a member; an edge is I when
//   both sides are members, B when one is; a vertex is I when every
//   triangle round it is a member and it is not on the hull, E when none
//   is, B otherwise.
// - dimension 1: an edge is I when it is a member; a vertex is E when no
//   member edge ends there, B when an odd number do (the mod-2 rule), I
//   otherwise; triangles are E.
// - dimension 0: a vertex is I when it is a member; everything else is E.
// Each matrix entry is the largest dimension of a cell in both classes.
[[cpp11::register]]
strings overlay_relate_cpp(integers t0, integers t1, integers t2, int n_vertex,
                           integers nbr, integers g_dim, integers g_start,
                           integers g_item, integers g_item2,
                           integers a, integers b) {
  const int nt = t0.size();
  const long long nv = n_vertex;
  auto corner = [&](int t, int k) {
    return (k == 0 ? t0[t] : (k == 1 ? t1[t] : t2[t])) - 1;
  };

  // undirected edges: id, endpoints, the triangles on either side
  std::vector<int> eid(3 * (size_t) nt);
  std::vector<int> e_t0, e_t1, e_v0, e_v1;
  std::unordered_map<long long, int> edge_of;
  edge_of.reserve(3 * (size_t) nt);
  for (int t = 0; t < nt; t++) {
    for (int k = 0; k < 3; k++) {
      const int u = corner(t, k), v = corner(t, (k + 1) % 3);
      const long long key = edge_key(u, v, nv);
      auto it = edge_of.find(key);
      if (it == edge_of.end()) {
        const int id = e_t0.size();
        edge_of.emplace(key, id);
        e_t0.push_back(t);
        e_t1.push_back(-1);
        e_v0.push_back(u);
        e_v1.push_back(v);
        eid[3 * t + k] = id;
      } else {
        e_t1[it->second] = t;
        eid[3 * t + k] = it->second;
      }
    }
  }
  const int ne = e_t0.size();

  // triangles round each vertex, and hull vertices
  std::vector<int> vt_start(nv + 1, 0), vt(3 * (size_t) nt);
  std::vector<char> hull(nv, 0);
  for (int t = 0; t < nt; t++)
    for (int k = 0; k < 3; k++) vt_start[corner(t, k) + 1]++;
  for (int v = 0; v < nv; v++) vt_start[v + 1] += vt_start[v];
  {
    std::vector<int> fill(vt_start.begin(), vt_start.end() - 1);
    for (int t = 0; t < nt; t++)
      for (int k = 0; k < 3; k++) vt[fill[corner(t, k)]++] = t;
  }
  for (int t = 0; t < nt; t++)
    for (int k = 0; k < 3; k++)
      if (nbr[3 * t + k] == 0) {
        hull[corner(t, k)] = 1;
        hull[corner(t, (k + 1) % 3)] = 1;
      }

  // members of each group as cell ids (triangle, edge or vertex, 0-based)
  const int ng = g_dim.size();
  std::vector<int> cell_start(ng + 1, 0), cell;
  cell.reserve(g_item.size());
  for (int g = 0; g < ng; g++) {
    for (int q = g_start[g] - 1; q < g_start[g + 1] - 1; q++) {
      if (g_dim[g] == 1) {
        auto it = edge_of.find(edge_key(g_item[q] - 1, g_item2[q] - 1, nv));
        if (it == edge_of.end()) {
          cpp11::stop("a line piece is not an edge of the mesh");
        }
        cell.push_back(it->second);
      } else {
        cell.push_back(g_item[q] - 1);
      }
    }
    // no member twice, so that edge counts at vertices are right
    std::sort(cell.begin() + cell_start[g], cell.end());
    cell.erase(std::unique(cell.begin() + cell_start[g], cell.end()), cell.end());
    cell_start[g + 1] = cell.size();
  }

  const int np = a.size();
  writable::strings out(np);
  // per side: membership stamps and edge counts at vertices
  std::vector<int> t_mark[2], e_mark[2], v_mark[2], v_cnt[2];
  for (int s = 0; s < 2; s++) {
    t_mark[s].assign(nt, -1);
    e_mark[s].assign(ne, -1);
    v_mark[s].assign(nv, -1);
    v_cnt[s].assign(nv, 0);
  }
  std::vector<int> seen_t(nt, -1), seen_e(ne, -1), seen_v(nv, -1);

  for (int p = 0; p < np; p++) {
    const int gg[2] = {a[p] - 1, b[p] - 1};
    int dimg[2];
    for (int s = 0; s < 2; s++) {
      const int g = gg[s];
      dimg[s] = g_dim[g];
      for (int q = cell_start[g]; q < cell_start[g + 1]; q++) {
        const int c = cell[q];
        if (dimg[s] == 2) {
          t_mark[s][c] = p;
        } else if (dimg[s] == 1) {
          e_mark[s][c] = p;
          const int ends[2] = {e_v0[c], e_v1[c]};
          for (int v : ends) {
            if (v_mark[s][v] != p) {
              v_mark[s][v] = p;
              v_cnt[s][v] = 0;
            }
            v_cnt[s][v]++;
          }
        } else {
          v_mark[s][c] = p;
        }
      }
    }
    // classes: 0 interior, 1 boundary, 2 exterior
    auto tri_class = [&](int s, int t) {
      return (dimg[s] == 2 && t_mark[s][t] == p) ? 0 : 2;
    };
    auto edge_class = [&](int s, int e) {
      if (dimg[s] == 2) {
        const int n_in = (t_mark[s][e_t0[e]] == p) +
          (e_t1[e] >= 0 && t_mark[s][e_t1[e]] == p);
        return n_in == 2 ? 0 : (n_in == 1 ? 1 : 2);
      }
      if (dimg[s] == 1) return e_mark[s][e] == p ? 0 : 2;
      return 2;
    };
    auto vert_class = [&](int s, int v) {
      if (dimg[s] == 2) {
        int n = 0, k = 0;
        for (int q = vt_start[v]; q < vt_start[v + 1]; q++) {
          n++;
          k += t_mark[s][vt[q]] == p;
        }
        if (k == 0) return 2;
        return (k == n && !hull[v]) ? 0 : 1;
      }
      if (dimg[s] == 1) {
        if (v_mark[s][v] != p || v_cnt[s][v] == 0) return 2;
        return (v_cnt[s][v] % 2 == 1) ? 1 : 0;
      }
      return v_mark[s][v] == p ? 0 : 2;
    };

    int dim[3][3];
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++) dim[i][j] = -1;
    dim[2][2] = 2;
    auto see = [&](int ca, int cb, int d) { if (d > dim[ca][cb]) dim[ca][cb] = d; };
    auto see_vertex = [&](int v) {
      if (seen_v[v] == p) return;
      seen_v[v] = p;
      see(vert_class(0, v), vert_class(1, v), 0);
    };
    auto see_edge = [&](int e) {
      if (seen_e[e] == p) return;
      seen_e[e] = p;
      see(edge_class(0, e), edge_class(1, e), 1);
      see_vertex(e_v0[e]);
      see_vertex(e_v1[e]);
    };
    // every cell in the closure of either group
    for (int s = 0; s < 2; s++) {
      const int g = gg[s];
      for (int q = cell_start[g]; q < cell_start[g + 1]; q++) {
        const int c = cell[q];
        if (dimg[s] == 2) {
          if (seen_t[c] != p) {
            seen_t[c] = p;
            see(tri_class(0, c), tri_class(1, c), 2);
          }
          for (int k = 0; k < 3; k++) see_edge(eid[3 * c + k]);
        } else if (dimg[s] == 1) {
          see_edge(c);
        } else {
          see_vertex(c);
        }
      }
    }
    std::string str(9, 'F');
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        if (dim[i][j] >= 0) str[3 * i + j] = (char) ('0' + dim[i][j]);
    out[p] = str;
  }
  return out;
}
