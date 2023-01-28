package Natural;

public class Negative implements Natural {
    Natural nat;

    public Negative(Natural nat) {
        this.nat = nat;
    }

    @Override
    public int naturalToInt() {
        return (-1) + nat.naturalToInt();
    }

}