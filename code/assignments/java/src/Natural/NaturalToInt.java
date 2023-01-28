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

    }
}
