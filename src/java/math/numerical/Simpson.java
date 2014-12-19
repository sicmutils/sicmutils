package math.numerical;

import clojure.lang.IFn;

public class Simpson {
    final static int jmax = 20;

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
            //System.out.println("j " + j + " os " + os + " ost " + ost);
        }
        throw new IllegalStateException("Too many steps in Simpson.integrate");
    }
}
