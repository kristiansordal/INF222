package BTL;

public class BTL1 {
    public static void main(String[] args) throws Exception {
        Z zero = new Z();
        I one = new I(zero);
        I two = new I(one);
        I three = new I(two);
        Plus p = new Plus(two, three);
        Mult m = new Mult(new Plus(two, three), new Mult(two, three));

        System.out.println(three);
        System.out.println(p.eval());
        System.out.println(m.eval().naturalToInt());

        System.out.println(m.isNormalForm());
        System.out.println(new I(new I(new I(new Z()))).isNormalForm());

        System.out.println(Expr.plus(two, three).eval1());
        System.out.println(Expr.mult(two, three));
        System.out.println(Expr.mult(three, three));
    }

}
