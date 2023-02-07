# Metaprogramming in Java - BTL Explained

In Haskell, we can construct new Data types using the `data` keyword. We give our data a name, and then list what is best described as *instances* of our data, with their respective (possibly different amount) constructors. An example of a data type would be the data type for `Natural`, which would look something like this:

```Haskell
data Natural 
    = Zero 
    | Succ Natural
    | Negative Natural
  deriving (Show, Eq, Read)
```

In Java, this is equivalent to creating an `interface`, and the instances of the datatype are equivalent to creating classes which implements the interface, using the *implements* keyword. So taking a look at the `Succ` instance, this would be a class named `Succ`, implementing the `Natural` interface.

What about the constructor for `Succ`? The Java equivalent here would be a field variable with of type `Natural`. After implementing this in Java, we would now have two files, as in the example below. For `Natural.java`, we would have

```Java
public interface Natural {

}
```

And for `Succ.java`, we would have

```Java
public class Succ implements Natural {
    Natural n;

    public Succ (Natural n) {
        this.n = n;
    }
}
```

We can also create a class for `Zero`, which would look like this:

```Java
public class Zero implements Natural {
@Override
    public int naturalToInt() {
        return 0;
    }

}
```

Now, what if we wanted to do something useful with this? Say, make a function. In `Haskell`, we might want to evaluate a Natural into an integer, with a `naturalToInt` function. This can be done with pattern matching and recursion. It would look something like this:

```Haskell
naturalToInt :: Natural -> Int
naturalToInt Zero = 0
naturalToInt (Succ n) = 1 + naturalToInt n
```

It's important to note its type: `Natural -> Int`. What would this look like in Java? Because the function takes in a `Natural`, we can in Java, define this as a method in our `Natural` interface, and we can pattern match in our respective classes. The `Natural` interface would now look like this:

```Java
public interface Natural {
    public int naturalToInt();
}
```

Note that the function doesn't take any argument. This will be clear when we override the method in the `Succ` and `Zero` class. For `Zero`, we end up with a method looking like this:

```Java
public class Zero implements Natural {
@Override
    public int naturalToInt() {
        return 0;
    }
}
```

And our `Succ` class will look like this:

```Java
public class Succ implements Natural {
    Natural n;

    public Succ (Natural n) {
        this.n = n;
    }

    @Override
    public int naturalToInt() {
        return 1 + nat.naturalToInt();
    }
}
```

Note the use of a field variable and a constructor, which eliminates the need for an argument in our function.
