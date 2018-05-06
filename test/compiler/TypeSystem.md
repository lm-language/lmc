```

MonoType =
	| TVar(n)
	| TGeneric(n)
	| Func(
		...MonoType(pn),
		...(Ident(ln), MonoType(ln)),
		MonoType(ret)
	)
Type =
	| MonoType
	| Forall(...TVar(gn), Type(t))
	| Func(
		...Type(pos),
		...(Ident(ln), Type(ln))
	)

Env =
	| empty
	| Env, (TVar(n1))
	| Env, (EVar(x) : Type)
	| Env, (TGeneric(n1))
	| Env, (TGeneric(n1): Type)
	| Env, |> Generic(n1)

-- Complete env
EnvC  =
	| empty
	| EnvC, (TVar(n))
	| EnvC, (EVar(x): Type)
	| EnvC, (TGeneric(n): MonoType(mt))
	| EnvC, |> Generic(n1)


Expr =
	| EVar(n)
	| Func(
		...TVar(g)
		...Pattern(p)
		Type(retType)?,
		Expr(body)
	 )
    | Call(
		Expr(f),
		...Expr(pos),
		...(Ident = Expr(l))
	  )
    | Error

```

Typing
======

```
// Expr(e) checks against
// Type(A)
|- Expr(e) <= Type(A)

// Expr(e) synthesizes
// type Type(A)
|- Expr(e) => Type(A)

// Expr(e) applied to a function of type Type(A)
// synthesizes type C
|- Type(A) . Expr(e) =>> Type(C)


// Apply current env to type T
[.]Type(T)

```

```
// Context application
[]TVar(a) == TVar(a)
[[TGen(a) = MonoType(t)]]TGen(a) ==
	[]MonoType(t)

[[TGen(a)]](TGen(a)) = TGen(a)
[]Func(
	...Type(posn),
	...(_, Type(ln)),
	Type(ret)
 =
  Func(
	...[]Type(posn),
	...(_, []Type(ln)),
	[]Type(ret)
  )

[]Forall(...TGen(n), Type(A)) =
	Forall(...TGen(n), []Type(A))

```

```

|- EVar(x) => Type(A)
when (
	|- (EVar(x): Type(A))
)

|- Expr(e) <= Type(B)
when (
	|- Expr(e) => A,
	|- []A <: []B
)

|- EAnnot(Expr(e), Type(A)) => Type(A)
when (
	|- Expr(e) <= Type(A)
)

|- e <= Forall(
	...TVar(an),
	Type(A)
) when (
	...TVar(an) |- Type(A) -| EnvC
)

|- Call(Expr(f), (
	...(Expr(pn)),
	...(Ident(l1), Expr(l1))
   ) => Type(Ret)
) when (
	|- (Expr(f) => (Forall(TVar(gpn), Type(A))))
	...Generic(gpn) |- [Generic(gpn)]
)

```