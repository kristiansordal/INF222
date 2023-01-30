package BTL;

import Natural.*;

public interface Expr {
    public Natural eval();

    public Expr eval1() throws Exception;

    public boolean isNormalForm();

    static public Expr plus(Expr x, Expr y) throws Exception {
        if ((x instanceof Z) && y.isNormalForm()) {
            return y;
        } else if (x instanceof I) {
            I i = (I) x;
            return new I(Expr.plus(i.e, y));
        }

        throw new Exception("Subarguments are not of normal form");

    }

    static public Expr mult(Expr x, Expr y) throws Exception {
        if ((x instanceof Z)) {
            return new Z();
        } else if (x instanceof I) {
            I i = (I) x;
            return Expr.plus(Expr.mult(i.e, y), y);
        }

        throw new Exception("Subarguments are not of normal form");

    }

    static public Expr monus(Expr x, Expr y) throws Exception {
        if ((x instanceof Z) && y.isNormalForm()) {
            return new Z();
        } else if (x instanceof I && y instanceof I) {
            I i1 = (I) x;
            I i2 = (I) y;
            return new I(Expr.monus(i1.e, i2.e));
        } else if ((y instanceof Z) && x.isNormalForm()) {
            return x;
        }

        throw new Exception("Subarguments are not of normal form");

    }
}
