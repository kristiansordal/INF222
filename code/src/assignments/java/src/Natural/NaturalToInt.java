package Natural;

public class NaturalToInt {
    public static void main(String[] args) {
        Zero z = new Zero();
        Succ s = new Succ(z);
        Succ s1 = new Succ(s);
        Negative n = new Negative(z);
        Negative n1 = new Negative(n);
        System.out.println(s.naturalToInt());
        System.out.println(s1.naturalToInt());
        System.out.println(n1.naturalToInt());
        System.out.println(Natural.intToNat(2).toString());
        System.out.println(Natural.plus(s1, s1));
        System.out.println(Natural.mult(Natural.mult(s1, s1), Natural.mult(s1, s1)));
        System.out.println(Natural.monus(s1, s1));

    }
}
