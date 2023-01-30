package BTL;

import Natural.*;

public class Plus implements Expr {
    Expr e1;
    Expr e2;

    public Plus(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public Natural eval() {
        return Natural.plus(e1.eval(), e2.eval());
    }

    @Override
    public boolean isNormalForm() {
        return false;
    }

    @Override
    public Expr eval1() throws Exception {
        return Expr.plus(e1.eval1(), e2.eval1());
    }

}
