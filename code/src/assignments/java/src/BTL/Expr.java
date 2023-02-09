package BTL;

import Natural.*;

public interface Expr {
    // Evaluate the expression with the semantic domain of the naturals.
    // Equivalent to the eval :: Expr -> ValueDomain Haskell funciton.
    public Natural eval() throws Exception;

    // Evaluate the expression with the semantic domain of Expr.
    // The semantic domain of Expr are the two constructors I and Z.
    // This is also known as the expressions normal form.
    // Equivalent to the eval' :: Expr -> ValueDomain' Haskell function.
    public Expr eval1() throws Exception;

    // Evaluate an expression to the semantic domain of the 16-bit
    // unsigned integers.
    // Not all children that implements the Expr class needs to make use of
    // eval2, it is therefore marked as default
    // Equivalent to the eval'''' :: Expr -> ValueDomain'''' Haskell function.
    public default short eval2() {
        return 0;
    }

    // Checks whether or not an expression is in its normal form.
    public boolean isNormalForm();

    static public Expr plus(Expr x, Expr y) throws Exception {
        if (!x.isNormalForm() || !y.isNormalForm()) {
            throw new Exception("Subarguments are not of normal form");
        }
        if ((x instanceof Z) && y.isNormalForm()) {
            return y;
        }
        I i = (I) x;
        return new I(Expr.plus(i.e, y));
    }

    static public Expr mult(Expr x, Expr y) throws Exception {
        if (!x.isNormalForm() || !y.isNormalForm()) {
            throw new Exception("Subarguments are not of normal form");
        }

        if ((x instanceof Z)) {
            return new Z();
        }
        I i = (I) x;
        return Expr.plus(Expr.mult(i.e, y), y);

    }

    static public Expr monus(Expr x, Expr y) throws Exception {
        if (!x.isNormalForm() || !y.isNormalForm()) {
            throw new Exception("Subarguments are not of normal form");
        }
        if ((x instanceof Z)) {
            return new Z();
        } else if (x instanceof I && y instanceof I) {
            I i1 = (I) x;
            I i2 = (I) y;
            return Expr.monus(i1.e, i2.e);
        }
        return x;
    }

    static public Expr and(Expr x, Expr y) throws Exception {
        if (!x.isNormalForm() || !y.isNormalForm()) {
            throw new Exception("Subarguments are not of normal form: e1 = " + x);
        }
        return (x instanceof T && y instanceof T) ? new T() : new F();
    }

    static public Expr or(Expr x, Expr y) throws Exception {
        if (!x.isNormalForm() || !y.isNormalForm()) {
            throw new Exception("Subarguments are not of normal form: e1 = " + x);
        }
        return (x instanceof F && y instanceof F) ? new F() : new T();
    }

    static public Expr not(Expr x) throws Exception {
        if (!x.isNormalForm()) {
            throw new Exception("Subarguments are not of normal form: e1 = " + x);
        }
        return (x instanceof T) ? new F() : new T();
    }

    static public Expr le(Expr x, Expr y) throws Exception {
        if (!x.isNormalForm() || !y.isNormalForm()) {
            throw new Exception("Subarguments are not of normal form: e1 = " + x);
        }
        if (x instanceof Z && y.isNormalForm()) {
            return new T();
        } else if (y instanceof Z && x.isNormalForm()) {
            return new F();
        }
        I i1 = (I) x;
        I i2 = (I) y;
        return Expr.le(i1.e, i2.e);
    }

    static public Expr ifte(Expr x, Expr y, Expr z) throws Exception {
        if (!y.isNormalForm() || !z.isNormalForm()) {
            throw new Exception("Subarguments are not of normal form: e1 = " + y);
        }
        if (x instanceof T && y.isNormalForm()) {
            return y;
        }
        return z;

    }
}
