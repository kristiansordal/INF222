package BTL;

import Natural.Natural;

public class Minus implements Expr {
    Expr e1;
    Expr e2;

    public Minus(Expr e1, Expr e2) {

        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public Natural eval() {
        return Natural.monus(e1.eval(), e2.eval());
    }

    @Override
    public boolean isNormalForm() {
        return false;
    }

    @Override
    public Expr eval1() throws Exception {
        return Expr.monus(e1.eval1(), e2.eval1());
    }
}
