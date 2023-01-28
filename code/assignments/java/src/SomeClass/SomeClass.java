package SomeClass;
public class SomeClass {
    public static void main(String[] args) {
        A testA = new A();
        testA.a = true;
        testA.b = true;
        testA.c = true;


        B testB = new B();
        testB.a = true;

        C testC = new C();

        System.out.println(testA.allTrue());
        System.out.println(testB.allTrue());
        System.out.println(testC.allTrue());
    }    
}

class A implements SomeType {
    boolean a;
    boolean b;
    boolean c;
    @Override
    public boolean allTrue() {
        return a && b && c;
    }
}

class B implements SomeType {
    boolean a;

    @Override
    public boolean allTrue() {
        return a;
    }
}

class C implements SomeType {
    @Override
    public boolean allTrue() {
        return false;
    }
}