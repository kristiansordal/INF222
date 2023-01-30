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
    public Natural eval() {
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
}
