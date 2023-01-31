package BTL;

import Natural.*;

public class T implements Expr {

    @Override
    public Natural eval() {
        return new Zero();
    }

    @Override
    public Expr eval1() throws Exception {
        return new T();
    }

    @Override
    public boolean isNormalForm() {
        return true;
    }

}
