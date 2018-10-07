package lmc

import lmc.diagnostics._
import lmc.Value.{Arrow, Constructor, Type, TypeOf, Uninferred}
import lmc.common.Symbol
import lmc.common.Scope

import scala.collection.mutable.ListBuffer

final class TypeChecker(
  private val ctx: Context.TC,
  private val env: Env
) {
  import lmc.syntax.{Parsed => P, Typed => T}

  private type E = Diagnostic => Unit

  private val Primitive = ctx.Primitive
  private val _values = collection.mutable.HashMap.empty[Symbol, Value]
  private val _types  = collection.mutable.HashMap.empty[Symbol, Value]
  private val _inferredTerms = collection.mutable.WeakHashMap.empty[P.Term, T.Term]

  for ((k, v) <- env.values) {
    _values.update(k, v)
  }
  for ((k, v) <- env.types) {
    _types.update(k, v)
  }

  def inferSourceFile(sourceFile: P.SourceFile, env: Env): T.SourceFile = {
    val declarations = sourceFile.declarations.map(inferDeclaration)
    val typ = getModuleType(sourceFile.scope)
    val inferredSourceFile = T.SourceFile(
      meta = sourceFile.meta.typed(typ),
      scope = sourceFile.scope,
      declarations = declarations
    )
    inferredSourceFile
  }

  private def inferDeclaration(declaration: P.Declaration): T.Declaration = {
    declaration match  {
      case l: P.Declaration.Let => inferLetDeclaration(l)
      case m: P.Declaration.Module => inferModuleDeclaration(m)
      case e: P.Declaration.Error =>
        T.Declaration.Error(e.meta.typed(Value.Uninferred))
    }
  }

  private def getModuleType(scope: Scope): Type = {
    Value.Module(
      scope.symbols.values
        .map(s => s -> getTypeOfSymbol(s))
        .toMap
    )
  }

  private def inferLetDeclaration(let: P.Declaration.Let): T.Declaration = {
    let match {
      case P.Declaration.Let(meta, pattern: P.Pattern.Var, Some(rhs)) =>
        val inferredRhs = inferTerm(rhs)
        val inferredPattern = checkPattern(pattern, inferredRhs.meta.typ)
        setValueOfSymbol(
          getPatternSymbol(inferredPattern),
          toValue(inferredRhs)
        )

        T.Declaration.Let(
          meta.typed(Primitive.Unit),
          inferredPattern,
          Some(inferredRhs)
        )
      case P.Declaration.Let(meta,  pattern: P.Pattern.Annotated, Some(rhs)) =>
        val typedPattern = inferPattern(pattern)
        val typedRhs = checkTerm(rhs, typedPattern.meta.typ)

        setValueOfSymbol(
          getPatternSymbol(typedPattern),
          toValue(typedRhs)
        )

        T.Declaration.Let(
          meta.typed(Primitive.Unit),
          typedPattern,
          Some(typedRhs)
        )
    }
  }

  private def inferTerm(term: P.Term): T.Term = {
    _inferredTerms.get(term) match {
      case Some(t) => t
      case None =>
        val t = term match {
          case f: P.Term.Func => inferFunc(f)
          case v: P.Term.Var => inferVarTerm(v)
          case i: P.Term.If => inferIfTerm(i)
          case c: P.Term.Call => inferCall(c)
          case t: P.Term.Literal => inferLiteral(t)
          case p: P.Term.Prop => inferProp(p)
          case m: P.Term.Module => inferModuleTerm(m)
          case t =>
            utils.todo("Match error: " + t.getClass.getName)
        }
        _inferredTerms.update(term, t)
        t
    }


  }

  private def inferProp(prop: P.Term.Prop): T.Term = {
    val typedLhs = inferTerm(prop.expr)
    val typ = Value.TypeOfMember(typedLhs.meta.typ, prop.prop.name)
    val typedRhs = T.Ident(
      prop.prop.meta.typed(typ),
      ctx.makeSymbol(prop.prop.name, P.Declaration.Error(prop.meta), prop)
    )
    T.Term.Prop(
      prop.meta.typed(typ),
      typedLhs,
      typedRhs
    )
  }

  private def inferLiteral(literal: P.Term.Literal): T.Term = {
    literal.variant match {
      case P.Term.Literal.LInt(value) =>
        T.Term.Literal(literal.meta.typed(Primitive.Int), T.Term.Literal.LInt(value))
    }
  }

  private def inferCall(c: P.Term.Call): T.Term.Call = {
    val typedFunc = inferTerm(c.func)
    val typedArgs = ListBuffer.empty[T.Term.Call.Arg]
    var currentReturnType: Type = typedFunc.meta.typ
    for (arg <- c.args) {
      val (typedArg, retTyp) = checkFunctionArg(arg, currentReturnType)
      currentReturnType = retTyp
      typedArgs.append(typedArg)

    }
    T.Term.Call(
      c.meta.typed(currentReturnType),
      typedFunc,
      typedArgs.toArray
    )
  }

  private def checkFunctionArg(arg: P.Term.Call.Arg, funcType: Type): (T.Term.Call.Arg, Type) = {
    funcType match {
      case Arrow(from, to) =>
        val typedTerm = checkTerm(arg.value, from)
        T.Term.Call.Arg(
          arg.meta.typed(typedTerm.meta.typ),
          None, // TODO: Handle named args
          typedTerm
        ) -> to
      case s if inNormalForm(funcType) =>
        utils.todo(s"$s is not a function")
      case _ =>
        checkFunctionArg(arg, reduce(funcType))
    }
  }

  private def reduce(value: Value): Value = {
    value match {
      case TypeOf(symbol) => getTypeOfSymbol(symbol)
    }
  }

  private def inferIfTerm(t: P.Term.If): T.Term.If = {
    val typedPredicate = checkTerm(t.predicate, Primitive.Bool)
    val typedTrueBranch = inferTerm(t.trueBranch)
    val typedFalseBranch = t.falseBranch.map(
      f => checkTerm(f, typedTrueBranch.meta.typ))
    val resultType = typedFalseBranch.map(_ => typedTrueBranch.meta.typ).getOrElse(Primitive.Unit)
    T.Term.If(
      t.meta.typed(resultType),
      typedPredicate,
      typedTrueBranch,
      typedFalseBranch
    )
  }

  private def inferVarTerm(term: P.Term.Var): T.Term.Var = {
    val typedIdent = inferIdent(term.ident)

    T.Term.Var(
      term.meta.typed(typedIdent.meta.typ),
      typedIdent
    )
  }

  private def inferIdent(value: P.Ident): T.Ident = {
    value.scope.resolve(value.name) match {
      case Some(symbol) =>
        T.Ident(
          value.meta.typed(TypeOf(symbol)),
          symbol
        )
      case None => utils.todo(s"Unbound var $value")
    }
  }

  private def inferFunc(f: P.Term.Func): T.Term = {
    val typedParams = f.params.map(param => {
      val p = inferPattern(param.pattern)
      T.Term.Param(p)
    })
    val typedReturnType = f.returnType.map(r => checkTerm(r, Primitive.Type))

    val typedBody = typedReturnType match {
      case Some(retTyp) => checkTerm(f.body, toValue(retTyp))
      case None => inferTerm(f.body)
    }
    val arrowTypes = (typedParams.map(_.pattern.meta.typ) :+ typedBody.meta.typ).toSeq
    val typ = Value.arrow(arrowTypes.head,  arrowTypes.tail: _*)
    T.Term.Func(
      f.meta.typed(typ),
      f.fnToken,
      f.funcScope,
      typedParams,
      typedReturnType,
      typedBody
    )
  }

  private def getPatternSymbol(pattern: T.Pattern): Symbol = {
    pattern match {
      case T.Pattern.Var(_, ident) => ident.name
      case T.Pattern.Annotated(_, inner, _) => getPatternSymbol(inner)
      case T.Pattern.Paren(_, inner) => getPatternSymbol(inner)
    }
  }

  private def inferPattern(pattern: P.Pattern): T.Pattern = {
    pattern match {
      case P.Pattern.Annotated(meta, innerPattern, typeAnnotation) =>
        val typedAnnotation = checkTerm(typeAnnotation, Primitive.Type)
        val typedPattern = checkPattern(innerPattern, toValue(typedAnnotation))
        T.Pattern.Annotated(
          meta.typed(typedPattern.meta.typ),
          typedPattern,
          typedAnnotation
        )
    }
  }

  private def checkPattern(pattern: P.Pattern, expectedType: Type): T.Pattern = {
    pattern match {
      case P.Pattern.Var(meta, ident) =>
        val typedIdent = checkBindingIdent(ident, expectedType)
        T.Pattern.Var(
          meta.typed(expectedType),
          typedIdent
        )
    }
  }

  private def checkBindingIdent(ident: P.Ident, expectedType: Type): T.Ident = {
    ident.scope.getSymbol(ident.name) match {
      case Some(s) =>
        setTypeOfSymbol(s, expectedType)
        T.Ident(
          ident.meta.typed(expectedType),
          s
        )
      case None => utils.todo(s"Compiler bug: No symbol for $ident")


    }
  }

  private def toValue(term: T.Term): Value = {
    term match {
      case T.Term.Var(_, ident) =>
        val result = getValueOfSymbol(ident.name).getOrElse(Value.Var(ident.name))
        result
      case f: T.Term.Func =>
        f.params.foldRight(toValue(f.body))((param, value) => Value.Func(arg =>
          subst(value, getPatternSymbol(param.pattern), arg)))
      case i: T.Term.If =>
        val predicateValue = toValue(i.predicate)
          Value.If(
            predicateValue,
            toValue(i.trueBranch),
            i.falseBranch.map(f => toValue(f)).getOrElse(Value.Unit)
          )
      case c: T.Term.Call =>
        val func = toValue(c.func)
        val args = c.args.map(arg => toValue(arg.value))
        val result = if (!args.forall(inNormalForm)) {
          args.foldLeft(func)(Value.Call.apply)
        } else {
          args.foldLeft(func)((f, arg) => f match {
            case Value.Func(func) =>
              func(arg)
            case _ => utils.todo(s"Not a function $f")
          })
        }
        result
      case T.Term.Literal(_, T.Term.Literal.LInt(value)) =>
        Value.Int(value)

      case p: T.Term.Prop =>
        Value.Prop(toValue(p.expr), p.prop.name.text)
    }
  }

  private var count = 0

  private def normalize(value: Value): Value = {
    count += 1
    if (count > 100) {
      throw new Error("overflow")
    }
    value match {
      case c: Constructor => c
      case b: Value.Bool => b
      case i: Value.Int => i
      case Value.Var(s) => (for {
        v <- getValueOfSymbol(s)
        _ = setTypeOfSymbol(s, v)
        t = normalize(v)
        _ = setTypeOfSymbol(s, t)
      } yield t).getOrElse(value)
      case Value.If(pred, trueBranch, falseBranch) =>
        normalize(pred) match {
          case Value.Bool(true) =>
            normalize(trueBranch)
          case Value.Bool(false) =>
            normalize(falseBranch)
          case p =>
            Value.If(p, trueBranch, falseBranch)
        }
      case Value.Call(func, arg) =>
        val normalizedFunc = normalize(func)
        val normalizedArg = normalize(arg)
        normalizedFunc match {
          case Value.Func(f) => normalize(f(normalizedArg))
          case _ => Value.Call(normalizedFunc, normalizedArg)
        }
      case Value.TypeOf(symbol) =>
        val result = normalize(getTypeOfSymbol(symbol))
        setTypeOfSymbol(symbol, result)
        result
      case Value.ModuleType(m) => Value.ModuleType(m.mapValues(normalize))
      case Value.TypeOfMember(lhs, rhs) =>
        resolvePropertySymbol(lhs, rhs) match {
          case Some(sym) => getNormalizedType(sym)
          case None =>
            utils.todo(s"$lhs has no property named $rhs")
        }
      case Value.Module(map) =>
        Value.Module(map.mapValues(normalize))
      case Value.Arrow(from, to) => Value.Arrow(normalize(from), normalize(to))

      case f: Value.Func =>
        f
    }
  }

  private def resolvePropertySymbol(value: Value, str: String): Option[Symbol] = {
    value match {
      case Value.Module(map) => map.keys.find(_.text == str)
      case Value.TypeOf(s) => resolvePropertySymbol(getValueOfSymbol(s).get, str)
      case Value.TypeOfMember(lhs, rhs) =>
        resolvePropertySymbol(lhs, rhs) match {
          case Some(s) => resolvePropertySymbol(getValueOfSymbol(s).get, str)
        }
    }
  }

  private def inNormalForm(value: Value): Boolean = {
    value match {
      case v: Value.Var =>
        getValueOfSymbol(v.symbol) match {
          case Some(other) => inNormalForm(other)
          case None => false
        }
      case _: Value.TypeOf => false
      case _: Value.Bool => true
      case _: Value.Int => true
    }
  }

  private def subst(value: Value, symbol: Symbol, arg: Value): Value = {
    def go(value: Value): Value = {
      subst(value, symbol, arg)
    }
    value match {
      case Value.Var(s) if s.id == symbol.id => arg
      case c: Value.Constructor => c
      case Value.Uninferred => value
      case Value.Call(c, arg) =>
        Value.Call(go(c), go(arg))
      case Value.If(pred, t, f) =>
        Value.If(go(pred), go(t), go(f))
      case f: Value.Func => f
      case b: Value.Bool => b
      case i: Value.Int => i
    }

  }

  private def checkTerm(term: P.Term, typ: Type): T.Term = {
    term match {
      case _ =>
        val typedTerm = inferTerm(term)
        if (typeEq(typ, typedTerm.meta.typ)) {
          typedTerm
        } else {
          typedTerm.withMeta(typedTerm.meta.withDiagnostic(
            Diagnostic(
              loc = term.loc,
              severity = Severity.Error,
              variant = TypeMismatch(typ, typedTerm.meta.typ)
            )
          ))
        }
    }
  }

  private def typeEq(expected: Type, found: Type): Boolean = {
    (expected, found) match {
      case (Constructor(s1), Constructor(s2)) => s1.id == s2.id
      case _ =>
        false
    }
  }

  private def inferModuleDeclaration(module: P.Declaration.Module): T.Declaration = {
    val typedDecls = module.body.map(inferDeclaration)
    val modTyp = Value.ModuleType(
      module.moduleScope.symbols.values.map(s => s -> getTypeOfSymbol(s)).toMap
    )
    val modValues = Value.Module(
      module.moduleScope
        .symbols.values
        .map(s => s -> getValueOfSymbol(s).getOrElse(Uninferred))
        .toMap
    )
    val typedIdent = checkBindingIdent(module.ident, modTyp)

    setValueOfSymbol(typedIdent.name, modValues)

    T.Declaration.Module(
      module.meta.typed(modTyp),
      typedIdent,
      module.scope,
      typedDecls
    )
  }

  private def inferModuleTerm(module: P.Term.Module): T.Term = {
    val typedDecls = module.declarations.map(inferDeclaration)
    val modTyp = Value.ModuleType(
      module.moduleScope.symbols.values.map(s => s -> getTypeOfSymbol(s)).toMap
    )
    T.Term.Module(
      module.meta.typed(modTyp),
      module.scope,
      typedDecls
    )
  }

  def getNormalizedType(symbol: Symbol): Type = {
    val t = normalize(getTypeOfSymbol(symbol))
    _types.update(symbol, t)
    t
  }

  private def getTypeOfSymbol(symbol: Symbol): Type = {
    _types.get(symbol) match {
      case Some(t) => t
      case None =>
        TypeOf(symbol)
    }
  }

  private def setTypeOfSymbol(symbol: Symbol, typ: Type): Unit = {
    _types.update(symbol, typ)
  }

  private def getValueOfSymbol(symbol: Symbol): Option[Value] = {
    _values.get(symbol)
  }

  private def setValueOfSymbol(symbol: Symbol, value: Value): Unit = {
    _values.update(symbol, value)
  }

}
