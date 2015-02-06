// Copyright (C) 2015 Colin Smith.
// This work is based on the Scmutils system of MIT/GNU Scheme.
//
// This is free software;  you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or (at
// your option) any later version.
//
// This software is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this code; if not, see <http://www.gnu.org/licenses/>.

package math.numerical;

import clojure.lang.IFn;

public class Simpson {
    final static int jmax = 20;

    private static class Trapezoid {
        private double s = 0.0;

        public double integrate(IFn f, double a, double b, int n) {
            double x, tnm, sum, del;
            int it, j;

            if (n == 1) {
                final double fa = (Double) f.invoke(a);
                final double fb = (Double) f.invoke(b);
                s = 0.5 * (b - a) * (fa + fb);
                return s;
            }
            for (it = 1, j = 1; j < n - 1; j++) it <<= 1; // weird: fix
            tnm = it;
            del = (b - a) / tnm;
            x = a + 0.5 * del;
            for (sum = 0.0, j = 0; j < it; j++, x += del) sum += (Double) f.invoke(x);
            s = 0.5 * (s + (b - a) * sum / tnm);
            return s;
        }
    }

    public static double integrate(double tol, IFn f, double a, double b) {
        Trapezoid trap = new Trapezoid();
        int j;
        double s, st, ost = 0, os = 0;
        for (j = 0; j < jmax; ++j) {
            st = trap.integrate(f, a, b, j+1);
            s = (4.0 * st - ost) / 3.0;
            if (j > 5) {
                if ((Math.abs(s-os) < tol * Math.abs(os)) ||
                        (Math.abs(s) < tol && Math.abs(os) < tol)) return s;
            }
            os = s;
            ost = st;
        }
        throw new IllegalStateException("Too many steps in Simpson.integrate");
    }
}
