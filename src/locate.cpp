// Point location on a triangle pool: the triangle that holds each query
// point, for tri_locate(). Triangles are bucketed on a uniform grid over
// the mesh extent (each triangle listed in every cell its bounding box
// covers, lowest triangle first), and the point-in-triangle test uses an
// exact orientation sign, so a point on an edge or vertex is decided the
// same way for every triangle that holds it. A point on an edge or vertex
// shared by several triangles goes to the lowest triangle row. Triangles
// may be either orientation; degenerate (zero area) triangles hold no
// point.
//
// The exact sign: a floating point determinant with an error bound
// (Shewchuk 1997, orient2d stage A), and when that cannot decide, the
// determinant expanded into six products of input coordinates, each split
// exactly into two doubles with fma(), summed exactly as a nonoverlapping
// expansion (two-sum), whose largest component has the sign of the sum.
// No product overflows or underflows for coordinates of ordinary size.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

using namespace cpp11;

namespace {

inline void two_sum(double a, double b, double& s, double& e) {
  s = a + b;
  double bv = s - a;
  double av = s - bv;
  e = (a - av) + (b - bv);
}

// add one double to a nonoverlapping expansion (increasing magnitude,
// zeros dropped)
void grow_expansion(std::vector<double>& e, double b) {
  std::vector<double> out;
  out.reserve(e.size() + 1);
  double q = b;
  for (double ei : e) {
    double s, h;
    two_sum(q, ei, s, h);
    if (h != 0.0) out.push_back(h);
    q = s;
  }
  if (q != 0.0) out.push_back(q);
  e.swap(out);
}

int orient_exact(double ax, double ay, double bx, double by, double cx, double cy) {
  // ax*by - ax*cy + bx*cy - bx*ay + cx*ay - cx*by
  const double f[6][2] = {{ax, by}, {-ax, cy}, {bx, cy}, {-bx, ay}, {cx, ay}, {-cx, by}};
  std::vector<double> e;
  e.reserve(12);
  for (int i = 0; i < 6; i++) {
    double p = f[i][0] * f[i][1];
    double r = std::fma(f[i][0], f[i][1], -p);
    grow_expansion(e, r);
    grow_expansion(e, p);
  }
  if (e.empty()) return 0;
  double top = e.back();
  return (top > 0) - (top < 0);
}

// sign of the area of a, b, c: 1 counter-clockwise, -1 clockwise, 0
// collinear
int orient_sign(double ax, double ay, double bx, double by, double cx, double cy) {
  const double detleft = (ax - cx) * (by - cy);
  const double detright = (ay - cy) * (bx - cx);
  const double det = detleft - detright;
  const double eps = std::numeric_limits<double>::epsilon() / 2;
  const double errbound = (3.0 + 16.0 * eps) * eps;
  double detsum;
  if (detleft > 0) {
    if (detright <= 0) return (det > 0) - (det < 0);
    detsum = detleft + detright;
  } else if (detleft < 0) {
    if (detright >= 0) return (det > 0) - (det < 0);
    detsum = -detleft - detright;
  } else {
    return (det > 0) - (det < 0);
  }
  if (det >= errbound * detsum || -det >= errbound * detsum) {
    return (det > 0) - (det < 0);
  }
  return orient_exact(ax, ay, bx, by, cx, cy);
}

}  // namespace

// x, y: vertex coordinates; t0, t1, t2: 1-based rows of the corners;
// px, py: query points. Returns the 1-based triangle holding each point,
// NA where none does.
[[cpp11::register]]
integers tri_locate_cpp(doubles x, doubles y, integers t0, integers t1, integers t2,
                        doubles px, doubles py) {
  const int nt = t0.size(), nq = px.size();
  writable::integers out(nq);
  for (int q = 0; q < nq; q++) out[q] = NA_INTEGER;
  if (nt == 0 || nq == 0) return out;

  std::vector<double> tx0(nt), tx1(nt), ty0(nt), ty1(nt);
  std::vector<signed char> sgn(nt);
  double xmin = std::numeric_limits<double>::infinity(), ymin = xmin;
  double xmax = -xmin, ymax = -xmin;
  for (int t = 0; t < nt; t++) {
    const int a = t0[t] - 1, b = t1[t] - 1, c = t2[t] - 1;
    tx0[t] = std::min(x[a], std::min(x[b], x[c]));
    tx1[t] = std::max(x[a], std::max(x[b], x[c]));
    ty0[t] = std::min(y[a], std::min(y[b], y[c]));
    ty1[t] = std::max(y[a], std::max(y[b], y[c]));
    xmin = std::min(xmin, tx0[t]); xmax = std::max(xmax, tx1[t]);
    ymin = std::min(ymin, ty0[t]); ymax = std::max(ymax, ty1[t]);
    sgn[t] = (signed char) orient_sign(x[a], y[a], x[b], y[b], x[c], y[c]);
  }

  const int ng = std::max(1, (int) std::sqrt((double) nt));
  const double cw = xmax > xmin ? (xmax - xmin) / ng : 1.0;
  const double ch = ymax > ymin ? (ymax - ymin) / ng : 1.0;
  auto cell = [&](double v, double lo, double w) {
    int c = (int) std::floor((v - lo) / w);
    return std::min(std::max(c, 0), ng - 1);
  };

  std::vector<int> start((size_t) ng * ng + 1, 0);
  std::vector<int> i0(nt), i1(nt), j0(nt), j1(nt);
  for (int t = 0; t < nt; t++) {
    i0[t] = cell(tx0[t], xmin, cw); i1[t] = cell(tx1[t], xmin, cw);
    j0[t] = cell(ty0[t], ymin, ch); j1[t] = cell(ty1[t], ymin, ch);
    for (int i = i0[t]; i <= i1[t]; i++)
      for (int j = j0[t]; j <= j1[t]; j++) start[(size_t) i * ng + j + 1]++;
  }
  for (size_t k = 1; k < start.size(); k++) start[k] += start[k - 1];
  std::vector<int> bucket(start.back());
  std::vector<int> fill(start.begin(), start.end() - 1);
  for (int t = 0; t < nt; t++)
    for (int i = i0[t]; i <= i1[t]; i++)
      for (int j = j0[t]; j <= j1[t]; j++) bucket[fill[(size_t) i * ng + j]++] = t;

  for (int q = 0; q < nq; q++) {
    const double qx = px[q], qy = py[q];
    if (std::isnan(qx) || std::isnan(qy)) continue;
    if (qx < xmin || qx > xmax || qy < ymin || qy > ymax) continue;
    const size_t k0 = (size_t) cell(qx, xmin, cw) * ng + cell(qy, ymin, ch);
    for (int k = start[k0]; k < start[k0 + 1]; k++) {
      const int t = bucket[k];
      if (sgn[t] == 0) continue;
      if (qx < tx0[t] || qx > tx1[t] || qy < ty0[t] || qy > ty1[t]) continue;
      const int a = t0[t] - 1, b = t1[t] - 1, c = t2[t] - 1;
      const int s = sgn[t];
      if (s * orient_sign(x[a], y[a], x[b], y[b], qx, qy) < 0) continue;
      if (s * orient_sign(x[b], y[b], x[c], y[c], qx, qy) < 0) continue;
      if (s * orient_sign(x[c], y[c], x[a], y[a], qx, qy) < 0) continue;
      out[q] = t + 1;
      break;
    }
  }
  return out;
}
