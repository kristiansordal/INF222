package BTL;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class BTL1Test {

    Expr expr0 = new Z();
    Expr expr1, expr2, expr3, expr4, expr5, expr6, expr7;

    @BeforeEach
    public void setup() {
        expr1 = BTL1Test.generate(3);
        try {
            expr2 = Expr.plus(Expr.mult(BTL1Test.generate(2), BTL1Test.generate(3)), BTL1Test.generate(1));
            expr3 = Expr.monus(expr2, expr1);
            expr4 = Expr.mult(expr1, Expr.plus(expr2, expr3));
            expr5 = Expr.plus(Expr.plus(expr0, expr1), Expr.monus(expr2, expr4));
            expr6 = Expr.monus(new I(Expr.mult(expr4, expr4)), Expr.plus(expr4, expr4));
            expr7 = Expr.ifte(Expr.le(expr1, expr3), expr5, expr6);
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    private static Expr generate(int n) {
        if (n == 0) {
            return new Z();
        } else
            return new I(generate(n - 1));
    }

    @Test
    void expr0() throws Exception {
        assertEquals(0, expr0.eval().naturalToInt());
    }

    @Test
    void expr1() throws Exception {
        assertEquals(3, expr1.eval().naturalToInt());
    }

    @Test
    void expr2() throws Exception {
        assertEquals(7, expr2.eval().naturalToInt());
    }

    @Test
    void expr3() throws Exception {
        assertEquals(4, expr3.eval().naturalToInt());
    }

    @Test
    void expr4() throws Exception {
        assertEquals(33, expr4.eval().naturalToInt());
    }

    @Test
    void expr5() throws Exception {
        assertEquals(3, expr5.eval().naturalToInt());
    }

    @Test
    void expr6() throws Exception {
        assertEquals(1024, expr6.eval().naturalToInt());
    }

    @Test
    void expr7() throws Exception {
        System.out.println(expr7.eval().naturalToInt());
        assertEquals(3, expr7.eval().naturalToInt());
    }
}
