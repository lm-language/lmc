Modules
=======

Modules are first class values that contain declarations mapping names to values.

They're similar to objects in most other languages except they are first class and can contain
both types and values (there isn't any difference between types and values).

```
let m1 = module {
    x = 45;
    y = "foo";
    T = Int;
};
```


Type of module can be written out using `module type`.

```
let M1Type = module type {
    x: Int;
    y: String;
    T: Type;
};
let m1: M1Type = <same as last example>;
```

Since we're can use `module type` terms in type annotations, they have the type `Type`.
(The only terms allowed in annotations are those having the type `Type`).

## Accessing values

Dot operator can be used to access module members

```
let x = m1.x;
```


## Generic modules

Since modules/module types are first class terms, they can be passed and returned from functions.

```
let Pair = fn(X: Type, Y: Type) => module type {
    x: X;
    y: Y;
};

let makePair = fn(X: Type, Y: Type, _x: X, _y: Y): Pair(X, Y) => module {
    let x = _x;
    let y = _y;
};

let p: Pair(Int, Bool) = makePair(Int, Bool, 1, true);

```

Calling `Pair` with two arguments that are types gives us the type of a pair with those
two properties.

To reduce boilerplate,  you can use the type argument syntax to make calling `makePair`
less verbose. Furthermore, you can omit `let` in module literals.

```
let makePair = fn[X, Y](_x: X, _y: Y) => module {
    x = _x;
    y = _y;
};

// everything in square brackets is inferred so this will expand to Pair(Int, Bool)
let p = makePair(1, true);

// It can still call this function the normal way
let p = makePair(Int, Bool, 1, true);

// or curried
let p = makePair(Int, Bool)(1, true);

```

Modules are structurally typed so there's nothing special about the `makePair` function or the `Pair` type.
Any type that has matching properties is assignable to `Pair(A, B)`.

For nominal typing, records are useful, and they also allow a cleaner syntax for data type declaration.
