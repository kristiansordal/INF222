package BTL;

import Natural.Natural;

public class Mult implements Expr {
    Expr e1;
    Expr e2;

    public Mult(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public Natural eval() throws Exception {
        return Natural.mult(e1.eval(), e2.eval());
    }

    @Override
    public boolean isNormalForm() {
        return false;
    }

    @Override
    public Expr eval1() throws Exception {
        return Expr.mult(e1.eval1(), e2.eval1());
    }

    @Override
    public short eval2() {
        return (short) ((short) e1.eval2() * e2.eval2());
    }
}
