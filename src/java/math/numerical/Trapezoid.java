package math.numerical;

import clojure.lang.IFn;

public class Trapezoid {
    private double s = 0.0;

    public double integrate(IFn f, double a, double b, int n) {
        double x, tnm, sum, del;
        int it, j;

        if (n == 1) {
            s = 0.5 * (b - a) * ((double) f.invoke(a) + (double) f.invoke(b));
            return s;
        }
        for (it = 1, j = 1; j < n - 1; j++) it <<= 1; // weird: fix
        tnm = it;
        del = (b - a) / tnm;
        x = a + 0.5 * del;
        for (sum = 0.0, j = 0; j < it; j++, x += del) sum += (double) f.invoke(x);
        s = 0.5 * (s + (b - a) * sum / tnm);
        return s;
    }
}
