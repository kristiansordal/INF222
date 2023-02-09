package BTL;

import Natural.*;

public class And implements Expr {

    Expr e1;
    Expr e2;

    public And(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public Natural eval() throws Exception {
        return ((Expr.plus(e1, e2)) instanceof Zero) ? new Zero() : new Succ(new Zero());
    }

    @Override
    public Expr eval1() throws Exception {
        return Expr.and(e1.eval1(), e2.eval1());
    }

    @Override
    public boolean isNormalForm() {
        return false;
    }
}
