package BTL;

public class BTL1 {
    public static void main(String[] args) throws Exception {
        Z zero = new Z();
        I one = new I(zero);
        I two = new I(one);
        I three = new I(two);
        I four = new I(three);
        I five = new I(four);
        Plus p = new Plus(two, three);
        Mult m = new Mult(new Plus(two, three), new Mult(two, three));

        // System.out.println(three);
        // System.out.println(p.eval());
        // System.out.println(m.eval().naturalToInt());

        // System.out.println(m.isNormalForm());
        // System.out.println(new I(new I(new I(new Z()))).isNormalForm());

        System.out.println(zero.eval());
        // System.out.println(Expr.plus(two, three).eval1());
        // System.out.println(Expr.plus(two, three).eval2());
        // System.out.println(Expr.mult(two, three).eval2());
        // System.out.println(Expr.mult(Expr.mult(two, three), Expr.plus(five,
        // five)).eval2());
        // System.out.println(Expr.monus(three, two).eval2());
    }
}
