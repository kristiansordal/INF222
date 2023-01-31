package BTL;

import Natural.*;

public class Not implements Expr {

    Expr e;

    public Not(Expr e) {
        this.e = e;
    }

    @Override
    public Natural eval() throws Exception {
        return (e.eval() instanceof Zero) ? new Succ(new Zero()) : new Zero();
    }

    @Override
    public Expr eval1() throws Exception {
        return Expr.not(e.eval1());
    }

    @Override
    public boolean isNormalForm() {
        return false;
    }

}
