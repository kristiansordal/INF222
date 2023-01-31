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
    public Natural eval() throws Exception {
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

    @Override
    public short eval2() {
        short s1 = e1.eval2();
        short s2 = e2.eval2();

        return (short) (s1 >= s2 ? s1 - s2 : 0);
    }
}
