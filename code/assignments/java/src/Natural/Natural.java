package Natural;

public interface Natural {
    public int naturalToInt();

    static public Natural intToNat(int n) {
        if (n == 0) {
            return new Zero();
        }
        return new Succ(intToNat(n - 1));
    }

    static public Natural plus(Natural x, Natural y) {
        return intToNat(x.naturalToInt() + y.naturalToInt());
    }

    static public Natural monus(Natural x, Natural y) {
        int n = x.naturalToInt() - y.naturalToInt();
        return n < 0 ? new Zero() : intToNat(n);
    }

    static public Natural mult(Natural x, Natural y) {
        return intToNat(x.naturalToInt() * y.naturalToInt());
    }
}