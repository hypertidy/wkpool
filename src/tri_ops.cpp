// Kernels for the tri_*() functions on a triangle pool: the edge table,
// per-group measures, DE-9IM between groups from the indices, boundary
// edges, clipping by a rectangle and convex hulls.
//
// Conventions (all set up by the R callers in R/tri_ops.R):
// - t0, t1, t2: 1-based rows into the vertex table (x, y), one triangle per
//   element, counter-clockwise.
// - nbr: the edge table, 3 * nt values; position 3 * t + k (0-based t)
//   holds the 1-based triangle across the edge from corner k to corner
//   k + 1, or 0 at the hull.
// - membership of triangles in groups as offsets: the groups of triangle t
//   (0-based) are mem[mem_start[t] - 1 .. mem_start[t + 1] - 2], 1-based
//   group ids; grp_start / grp_tri is the same inverted (triangles of each
//   group, 1-based). A coverage has one group per triangle; overlapping
//   groups list a triangle under each group that covers it.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <string>
#include <vector>

using namespace cpp11;

namespace {

int corner(const integers& t0, const integers& t1, const integers& t2, int t, int k) {
  return (k == 0 ? t0[t] : (k == 1 ? t1[t] : t2[t])) - 1;
}

bool has_group(const integers& mem_start, const integers& mem, int t, int g) {
  for (int k = mem_start[t] - 1; k < mem_start[t + 1] - 1; k++) {
    if (mem[k] == g) return true;
  }
  return false;
}

}  // namespace

// The edge table: for each triangle edge the triangle on its other side.
// Two triangles are neighbours across an edge when they hold both its
// vertices in opposite order; an edge used by more than two triangles, or
// twice in the same direction, has no neighbour.
[[cpp11::register]]
integers tri_neighbours_cpp(integers t0, integers t1, integers t2) {
  const int nt = t0.size();
  struct E { int lo, hi, dir, h; };
  std::vector<E> e(3 * (size_t) nt);
  for (int t = 0; t < nt; t++) {
    for (int k = 0; k < 3; k++) {
      int a = corner(t0, t1, t2, t, k), b = corner(t0, t1, t2, t, (k + 1) % 3);
      e[3 * t + k] = {std::min(a, b), std::max(a, b), a < b ? 1 : 0, 3 * t + k};
    }
  }
  std::sort(e.begin(), e.end(), [](const E& p, const E& q) {
    if (p.lo != q.lo) return p.lo < q.lo;
    if (p.hi != q.hi) return p.hi < q.hi;
    return p.h < q.h;
  });
  writable::integers out(3 * (R_xlen_t) nt);
  for (R_xlen_t i = 0; i < out.size(); i++) out[i] = 0;
  size_t i = 0;
  while (i < e.size()) {
    size_t j = i + 1;
    while (j < e.size() && e[j].lo == e[i].lo && e[j].hi == e[i].hi) j++;
    if (j - i == 2 && e[i].dir != e[i + 1].dir) {
      out[e[i].h] = e[i + 1].h / 3 + 1;
      out[e[i + 1].h] = e[i].h / 3 + 1;
    }
    i = j;
  }
  return out;
}

// Per group: area, perimeter, area-weighted centroid, the centroid of the
// largest triangle (a point inside the group) and the extent. Returns an
// n_group x 10 matrix in that order (area, perimeter, cx, cy, px, py,
// xmin, ymin, xmax, ymax).
[[cpp11::register]]
doubles_matrix<> tri_measures_cpp(doubles x, doubles y, integers t0, integers t1, integers t2,
                                  integers nbr, integers mem_start, integers mem,
                                  int n_group) {
  const int nt = t0.size();
  writable::doubles_matrix<> out(n_group, 10);
  std::vector<double> amax(n_group, -1);
  for (int g = 0; g < n_group; g++) {
    for (int j = 0; j < 10; j++) out(g, j) = 0;
    out(g, 4) = NA_REAL; out(g, 5) = NA_REAL;
    out(g, 6) = R_PosInf; out(g, 7) = R_PosInf; out(g, 8) = R_NegInf; out(g, 9) = R_NegInf;
  }
  for (int t = 0; t < nt; t++) {
    const int a = t0[t] - 1, b = t1[t] - 1, c = t2[t] - 1;
    const double ar = 0.5 * ((x[b] - x[a]) * (y[c] - y[a]) - (x[c] - x[a]) * (y[b] - y[a]));
    const double cx = (x[a] + x[b] + x[c]) / 3, cy = (y[a] + y[b] + y[c]) / 3;
    for (int k = mem_start[t] - 1; k < mem_start[t + 1] - 1; k++) {
      const int g = mem[k] - 1;
      out(g, 0) += ar;
      out(g, 2) += ar * cx;
      out(g, 3) += ar * cy;
      if (ar > amax[g]) { amax[g] = ar; out(g, 4) = cx; out(g, 5) = cy; }
      const int vs[3] = {a, b, c};
      for (int e = 0; e < 3; e++) {
        const int v = vs[e];
        if (x[v] < out(g, 6)) out(g, 6) = x[v];
        if (y[v] < out(g, 7)) out(g, 7) = y[v];
        if (x[v] > out(g, 8)) out(g, 8) = x[v];
        if (y[v] > out(g, 9)) out(g, 9) = y[v];
        const int u = nbr[3 * t + e] - 1;
        if (u >= 0 && has_group(mem_start, mem, u, g + 1)) continue;
        const int p = vs[e], q = vs[(e + 1) % 3];
        out(g, 1) += std::sqrt((x[q] - x[p]) * (x[q] - x[p]) + (y[q] - y[p]) * (y[q] - y[p]));
      }
    }
  }
  for (int g = 0; g < n_group; g++) {
    if (out(g, 0) != 0) {
      out(g, 2) /= out(g, 0);
      out(g, 3) /= out(g, 0);
    } else {
      out(g, 2) = NA_REAL; out(g, 3) = NA_REAL;
    }
    if (!(amax[g] >= 0)) {
      for (int j = 6; j < 10; j++) out(g, j) = NA_REAL;
    }
  }
  return out;
}

// Pairs of groups that share a vertex: the candidates for every relation
// but disjoint. A two-column matrix (a, b), a < b, sorted.
[[cpp11::register]]
integers_matrix<> tri_vertex_pairs_cpp(integers t0, integers t1, integers t2,
                                       integers mem_start, integers mem) {
  const int nt = t0.size();
  std::vector<std::pair<int, int>> vg;
  vg.reserve(3 * (size_t) nt);
  for (int t = 0; t < nt; t++) {
    for (int k = mem_start[t] - 1; k < mem_start[t + 1] - 1; k++) {
      vg.emplace_back(t0[t], mem[k]);
      vg.emplace_back(t1[t], mem[k]);
      vg.emplace_back(t2[t], mem[k]);
    }
  }
  std::sort(vg.begin(), vg.end());
  vg.erase(std::unique(vg.begin(), vg.end()), vg.end());
  std::vector<std::pair<int, int>> pr;
  size_t i = 0;
  while (i < vg.size()) {
    size_t j = i;
    while (j < vg.size() && vg[j].first == vg[i].first) j++;
    for (size_t p = i; p < j; p++)
      for (size_t q = p + 1; q < j; q++) pr.emplace_back(vg[p].second, vg[q].second);
    i = j;
  }
  std::sort(pr.begin(), pr.end());
  pr.erase(std::unique(pr.begin(), pr.end()), pr.end());
  writable::integers_matrix<> out(pr.size(), 2);
  for (size_t k = 0; k < pr.size(); k++) {
    out(k, 0) = pr[k].first;
    out(k, 1) = pr[k].second;
  }
  return out;
}

// DE-9IM between pairs of groups, from the indices alone. Every point of
// the plane is in exactly one open triangle, open edge or vertex of the
// mesh, or outside it, and each of those cells lies wholly in the
// interior, boundary or exterior of a group:
// - a triangle is interior when it is a member, else exterior;
// - an edge is interior when the triangles on both sides are members,
//   boundary when one is (outside the mesh is never a member), exterior
//   when neither is;
// - a vertex is interior when every triangle around it is a member and it
//   is not on the hull, exterior when none is, boundary otherwise.
// Entry (class in A, class in B) is the largest dimension of a cell in
// both classes (2 triangle, 1 edge, 0 vertex, F none); EE is always 2,
// outside the mesh. Only cells of the two groups' triangles can be other
// than exterior to both, so a pair costs the size of its groups.
[[cpp11::register]]
strings tri_relate_cpp(integers t0, integers t1, integers t2, int n_vertex, integers nbr,
                       integers mem_start, integers mem, integers grp_start,
                       integers grp_tri, integers a, integers b) {
  const int nt = t0.size();
  // triangles around each vertex, and vertices on the hull
  std::vector<int> vt_start(n_vertex + 1, 0), vt(3 * (size_t) nt);
  std::vector<char> hull(n_vertex, 0);
  for (int t = 0; t < nt; t++)
    for (int k = 0; k < 3; k++) vt_start[corner(t0, t1, t2, t, k) + 1]++;
  for (int v = 0; v < n_vertex; v++) vt_start[v + 1] += vt_start[v];
  {
    std::vector<int> fill(vt_start.begin(), vt_start.end() - 1);
    for (int t = 0; t < nt; t++)
      for (int k = 0; k < 3; k++) vt[fill[corner(t0, t1, t2, t, k)]++] = t;
  }
  for (int t = 0; t < nt; t++) {
    for (int k = 0; k < 3; k++) {
      if (nbr[3 * t + k] == 0) {
        hull[corner(t0, t1, t2, t, k)] = 1;
        hull[corner(t0, t1, t2, t, (k + 1) % 3)] = 1;
      }
    }
  }

  const int np = a.size();
  writable::strings out(np);
  std::vector<int> stamp_t(nt, -1), stamp_v(n_vertex, -1);
  for (int p = 0; p < np; p++) {
    const int ga = a[p], gb = b[p];
    int dim[3][3];
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++) dim[i][j] = -1;
    dim[2][2] = 2;
    auto member = [&](int t, int g) { return t >= 0 && has_group(mem_start, mem, t, g); };
    auto see = [&](int ca, int cb, int d) { if (d > dim[ca][cb]) dim[ca][cb] = d; };
    auto side_class = [](int n_in) { return n_in == 2 ? 0 : (n_in == 1 ? 1 : 2); };
    for (int which = 0; which < 2; which++) {
      const int g = which == 0 ? ga : gb;
      for (int k = grp_start[g - 1] - 1; k < grp_start[g] - 1; k++) {
        const int t = grp_tri[k] - 1;
        if (stamp_t[t] == p) continue;
        stamp_t[t] = p;
        const bool ta = member(t, ga), tb = member(t, gb);
        see(ta ? 0 : 2, tb ? 0 : 2, 2);
        for (int e = 0; e < 3; e++) {
          const int u = nbr[3 * t + e] - 1;
          const int ina = (int) ta + (int) member(u, ga);
          const int inb = (int) tb + (int) member(u, gb);
          see(side_class(ina), side_class(inb), 1);
          const int v = corner(t0, t1, t2, t, e);
          if (stamp_v[v] == p) continue;
          stamp_v[v] = p;
          int n = 0, ka = 0, kb = 0;
          for (int q = vt_start[v]; q < vt_start[v + 1]; q++) {
            n++;
            ka += member(vt[q], ga);
            kb += member(vt[q], gb);
          }
          const bool h = hull[v];
          auto vclass = [&](int kk) { return kk == 0 ? 2 : ((kk == n && !h) ? 0 : 1); };
          see(vclass(ka), vclass(kb), 0);
        }
      }
    }
    std::string s(9, 'F');
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        if (dim[i][j] >= 0) s[3 * i + j] = (char) ('0' + dim[i][j]);
    out[p] = s;
  }
  return out;
}

// Boundary edges of each group, directed with the group on the left:
// (group, from row, to row). With once = TRUE an edge between two groups
// is kept only for the smaller group id, so every boundary is listed once.
[[cpp11::register]]
integers_matrix<> tri_boundary_cpp(integers t0, integers t1, integers t2, integers nbr,
                                   integers mem_start, integers mem, bool once) {
  const int nt = t0.size();
  std::vector<int> g_out, a_out, b_out;
  for (int t = 0; t < nt; t++) {
    for (int k = mem_start[t] - 1; k < mem_start[t + 1] - 1; k++) {
      const int g = mem[k];
      for (int e = 0; e < 3; e++) {
        const int u = nbr[3 * t + e] - 1;
        if (u >= 0 && has_group(mem_start, mem, u, g)) continue;
        if (once && u >= 0) {
          // the edge also bounds the groups of u that are not in t
          int smallest = g;
          for (int j = mem_start[u] - 1; j < mem_start[u + 1] - 1; j++)
            if (!has_group(mem_start, mem, t, mem[j]) && mem[j] < smallest) smallest = mem[j];
          if (smallest != g) continue;
        }
        g_out.push_back(g);
        a_out.push_back(corner(t0, t1, t2, t, e) + 1);
        b_out.push_back(corner(t0, t1, t2, t, (e + 1) % 3) + 1);
      }
    }
  }
  writable::integers_matrix<> out(g_out.size(), 3);
  for (size_t i = 0; i < g_out.size(); i++) {
    out(i, 0) = g_out[i];
    out(i, 1) = a_out[i];
    out(i, 2) = b_out[i];
  }
  return out;
}

// Clip triangles by a rectangle. Triangles inside are kept, triangles
// outside dropped, and crossing triangles are cut by the four sides
// (Sutherland-Hodgman) and fanned back into triangles. A point where an
// edge crosses a side is computed from the edge's endpoints in a fixed
// order (input vertex rows ascending, else coordinates), so the two
// triangles that share an edge get bit-identical points. Returns one row
// per output triangle: source triangle (1-based), then for each corner
// the input vertex row (0 for a new point), x and y.
[[cpp11::register]]
doubles_matrix<> tri_clip_rect_cpp(doubles x, doubles y, integers t0, integers t1, integers t2,
                                   double xmin, double ymin, double xmax, double ymax) {
  const int nt = t0.size();
  struct P { double x, y; int id; };   // id >= 0: input vertex row (0-based)
  std::vector<double> o;
  auto inside = [&](const P& p, int side) {
    switch (side) {
      case 0: return p.x >= xmin;
      case 1: return p.x <= xmax;
      case 2: return p.y >= ymin;
      default: return p.y <= ymax;
    }
  };
  auto cut = [&](P p, P q, int side) {
    if (q.id >= 0 && (p.id < 0 || q.id < p.id)) {
      std::swap(p, q);
    } else if (q.id < 0 && p.id < 0 && (q.x < p.x || (q.x == p.x && q.y < p.y))) {
      std::swap(p, q);
    }
    P r;
    if (side < 2) {
      const double c = side == 0 ? xmin : xmax;
      r.x = c;
      r.y = p.y + (c - p.x) / (q.x - p.x) * (q.y - p.y);
    } else {
      const double c = side == 2 ? ymin : ymax;
      r.y = c;
      r.x = p.x + (c - p.y) / (q.y - p.y) * (q.x - p.x);
    }
    r.id = -1;
    return r;
  };
  for (int t = 0; t < nt; t++) {
    const int a = t0[t] - 1, b = t1[t] - 1, c = t2[t] - 1;
    const double tx0 = std::min({x[a], x[b], x[c]}), tx1 = std::max({x[a], x[b], x[c]});
    const double ty0 = std::min({y[a], y[b], y[c]}), ty1 = std::max({y[a], y[b], y[c]});
    if (tx1 < xmin || tx0 > xmax || ty1 < ymin || ty0 > ymax) continue;
    std::vector<P> poly = {{x[a], y[a], a}, {x[b], y[b], b}, {x[c], y[c], c}};
    if (!(tx0 >= xmin && tx1 <= xmax && ty0 >= ymin && ty1 <= ymax)) {
      for (int side = 0; side < 4 && !poly.empty(); side++) {
        std::vector<P> next;
        const int n = poly.size();
        for (int i = 0; i < n; i++) {
          const P& p = poly[i];
          const P& q = poly[(i + 1) % n];
          const bool pin = inside(p, side), qin = inside(q, side);
          if (pin) next.push_back(p);
          if (pin != qin) next.push_back(cut(p, q, side));
        }
        poly.swap(next);
      }
    }
    if (poly.size() < 3) continue;
    for (size_t i = 1; i + 1 < poly.size(); i++) {
      const P& p = poly[0];
      const P& q = poly[i];
      const P& r = poly[i + 1];
      const double ar = (q.x - p.x) * (r.y - p.y) - (r.x - p.x) * (q.y - p.y);
      if (ar <= 0) continue;   // a sliver along a side
      o.push_back(t + 1);
      const P* cs[3] = {&p, &q, &r};
      for (int k = 0; k < 3; k++) {
        o.push_back(cs[k]->id + 1);
        o.push_back(cs[k]->x);
        o.push_back(cs[k]->y);
      }
    }
  }
  const size_t n = o.size() / 10;
  writable::doubles_matrix<> out(n, 10);
  for (size_t i = 0; i < n; i++)
    for (int j = 0; j < 10; j++) out(i, j) = o[10 * i + j];
  return out;
}

// Convex hull of each group's vertices (Andrew's monotone chain): one row
// per hull vertex, counter-clockwise, not closed: (group, vertex row).
[[cpp11::register]]
integers_matrix<> tri_convex_hull_cpp(doubles x, doubles y, integers t0, integers t1,
                                      integers t2, integers grp_start, integers grp_tri,
                                      int n_group) {
  std::vector<int> g_out, v_out, pts, hull;
  for (int g = 0; g < n_group; g++) {
    pts.clear();
    for (int k = grp_start[g] - 1; k < grp_start[g + 1] - 1; k++) {
      const int t = grp_tri[k] - 1;
      pts.push_back(t0[t] - 1);
      pts.push_back(t1[t] - 1);
      pts.push_back(t2[t] - 1);
    }
    std::sort(pts.begin(), pts.end(), [&](int p, int q) {
      return x[p] < x[q] || (x[p] == x[q] && y[p] < y[q]);
    });
    pts.erase(std::unique(pts.begin(), pts.end(), [&](int p, int q) {
      return x[p] == x[q] && y[p] == y[q];
    }), pts.end());
    const int n = pts.size();
    if (n < 3) continue;
    hull.assign(2 * n, 0);
    int k = 0;
    auto cross = [&](int o, int p, int q) {
      return (x[p] - x[o]) * (y[q] - y[o]) - (y[p] - y[o]) * (x[q] - x[o]);
    };
    for (int i = 0; i < n; i++) {
      while (k >= 2 && cross(hull[k - 2], hull[k - 1], pts[i]) <= 0) k--;
      hull[k++] = pts[i];
    }
    for (int i = n - 2, lo = k + 1; i >= 0; i--) {
      while (k >= lo && cross(hull[k - 2], hull[k - 1], pts[i]) <= 0) k--;
      hull[k++] = pts[i];
    }
    for (int i = 0; i + 1 < k; i++) {
      g_out.push_back(g + 1);
      v_out.push_back(hull[i] + 1);
    }
  }
  writable::integers_matrix<> out(g_out.size(), 2);
  for (size_t i = 0; i < g_out.size(); i++) {
    out(i, 0) = g_out[i];
    out(i, 1) = v_out[i];
  }
  return out;
}
