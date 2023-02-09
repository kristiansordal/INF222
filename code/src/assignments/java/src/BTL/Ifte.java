package BTL;

import Natural.*;

public class Ifte implements Expr {

    Expr e1;
    Expr e2;
    Expr e3;

    public Ifte(Expr e1, Expr e2, Expr e3) {
        this.e1 = e1;
        this.e2 = e2;
        this.e3 = e3;
    }

    @Override
    public Natural eval() throws Exception {
        return (e1.eval() instanceof Zero) ? e2.eval() : e3.eval();
    }

    @Override
    public Expr eval1() throws Exception {
        return Expr.ifte(e1.eval1(), e2.eval1(), e3.eval1());

    }

    @Override
    public boolean isNormalForm() {
        return false;
    }

}
