package BTL;

import Natural.*;

public class F implements Expr {

    @Override
    public Natural eval() {
        return new Succ(new Zero());
    }

    @Override
    public Expr eval1() throws Exception {
        return new F();
    }

    @Override
    public boolean isNormalForm() {
        return true;
    }

}
