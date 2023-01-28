package Natural;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;
public class NaturalTest {

    @Test 
    void testZero(){
        Zero x = new Zero();
        assertEquals(x.naturalToInt(), 0);
    }

    @Test
    void testOne() {
        Zero z = new Zero();
        Succ n = new Succ(z);
        assertEquals(n.naturalToInt(), 1);
    }

    @Test
    void testNegOne() {
        Zero z = new Zero();
        Negative n = new Negative(z);
        assertEquals(n.naturalToInt(), -1);
    }

}
