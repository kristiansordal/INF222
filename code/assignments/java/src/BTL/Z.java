package BTL;

import Natural.*;

public class Z implements Expr {

    @Override
    public Natural eval() {
        return new Zero();
    }

    @Override
    public String toString() {
        return "Z";
    }

    @Override
    public boolean isNormalForm() {
        return true;
    }

    @Override
    public Expr eval1() {
        return new Z();
    }

}
