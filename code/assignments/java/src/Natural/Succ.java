package Natural;

public class Succ implements Natural {
    Natural nat;

    public Succ(Natural nat) {
        this.nat = nat;
    }

    @Override
    public int naturalToInt() {
        return 1 + nat.naturalToInt();
    }

    @Override
    public String toString() {
        return "Succ " + nat.toString();
    }
}