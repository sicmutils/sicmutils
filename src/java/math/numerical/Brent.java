package math.numerical;

import clojure.lang.IFn;

public class Brent {
    private static final double cgold = 0.5 * (3.0 - Math.sqrt(5.0));
    private static final double eps = 2.22e-16;
    private static final int itmax = 1000;

    public static class Result {
        public final double x;
        public final double fx;
        public final int iter;

        Result(double x, double fx, int iter) {
            this.x = x;
            this.fx = fx;
            this.iter = iter;
        }
    }

    public static Result minimize(final double a, final double b, final IFn f, final double t) {
        int iter = 0;
        double d = 0.0, e = 0.0, fu, fv, fw, fx, m, p, q, r, tol, t2, u, v, w;
        double sa = a < b ? a : b;
        double sb = a < b ? b : a;
        double x = sa + cgold * (b-a);
        w = x;
        v = w;
        fx = (Double) f.invoke(x);
        fw = fx;
        fv = fw;
        while (true) {
            m = 0.5 * (sa + sb);
            tol = eps * Math.abs(x) + t;
            t2 = 2.0 * tol;
            if (Math.abs(x-m) <= t2 - 0.5 * (sb - sa)) {
                return new Result(x, fx, iter);
            }
            if (iter > itmax) throw new IllegalArgumentException("Brent/minimize failed to converge");
            r = 0.0;
            q = r;
            p = q;
            if (tol < Math.abs(e)) {
                r = (x - w) * (fx - fv);
                q = (x - v) * (fx - fw);
                p = (x - v) * q - (x - w) * r;
                q = 2.0 * (q - r);
                if (0.0 < q) p = -p;
                q = Math.abs(q);
                r = e;
                e = d;
            }
            if (Math.abs(p) < Math.abs(0.5 * q * r) &&
                    q * (sa - x) < p &&
                    p < q * (sb - x)) {
                d = p / q;
                u = x + d;
                if ((u - sa) < t2 || (sb - u) < t2) {
                    if (x < m) d = tol; else d = -tol;
                }
            } else {
                if (x < m) e = sb - x; else e = sa - x;
                d = cgold * e;
            }
            if (tol < Math.abs(d)) {
                u = x + d;
            } else if (0.0 < d) {
                u = x + tol;
            } else {
                u = x - tol;
            }
            fu = (Double) f.invoke(u);
            if (fu <= fx) {
                if (u < x) sb = x; else sa = x;
                v = w;
                fv = fw;
                w = x;
                fw = fx;
                x = u;
                fx = fu;
            } else {
                if (u < x) sa = u; else sb = u;
                if (fu <= fw || w == x) {
                    v = w;
                    fv = fw;
                    w = u;
                    fw = fu;
                } else if (fu <= fv || v == x || v == w) {
                    v = u;
                    fv = fu;
                }
            }
            iter++;
        }
    }
}