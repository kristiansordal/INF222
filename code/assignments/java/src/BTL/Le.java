package BTL;

import Natural.*;

public class Le implements Expr {

    Expr e1;
    Expr e2;

    public Le(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public Natural eval() throws Exception {
        return (Natural.le(e1.eval(), e2.eval())) ? new Zero() : new Succ(new Zero());
    }

    @Override
    public Expr eval1() throws Exception {
        return Expr.le(e1.eval1(), e2.eval1());
    }

    @Override
    public boolean isNormalForm() {
        return false;
    }

}
