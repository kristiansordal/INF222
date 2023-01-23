
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

class Zero implements Natural {
    @Override
    public int naturalToInt() {
        return 0;
    }

}

class Succ implements Natural {
    Natural nat;

    public Succ(Natural nat) {
        this.nat = nat;
    }

    @Override
    public int naturalToInt() {
        return 1 + nat.naturalToInt();
    }

}

class Negative implements Natural {
    Natural nat;

    public Negative(Natural nat) {
        this.nat = nat;
    }

    @Override
    public int naturalToInt() {
        return (-1) + nat.naturalToInt();
    }

}
