package BTL;

import Natural.*;

public class I implements Expr {
    Expr e;

    public I(Expr e) {
        this.e = e;
    }

    @Override
    public Natural eval() throws Exception {
        return new Succ(e.eval());
    }

    @Override
    public boolean isNormalForm() {
        return e.isNormalForm();
    }

    @Override
    public Expr eval1() throws Exception {
        return new I(e.eval1());
    }

    @Override
    public String toString() {
        return "I " + e.toString();
    }

    @Override
    public short eval2() {
        return (short) (1 + e.eval2());
    }

}
