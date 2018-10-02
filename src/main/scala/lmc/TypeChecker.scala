package lmc

import lmc.diagnostics._
import lmc.Value.{Arrow, Constructor, Type, Uninferred}
import lmc.common.Symbol
import lmc.common.Scope

import scala.collection.mutable.ListBuffer

final class TypeChecker(
  private val ctx: Context.TC,
) {
  import lmc.syntax.{Parsed => P, Typed => T}

  private type E = Diagnostic => Unit

  private val Primitive = ctx.Primitive

  def inferSourceFile(sourceFile: P.SourceFile, env: Env): T.SourceFile = {
    var currentEnv = env
    val declarations = ListBuffer.empty[T.Declaration]
    for (decl <- sourceFile.declarations) {
      val (typedDecl, newEnv) = inferDeclaration(currentEnv)(decl)
      currentEnv = newEnv
      declarations.append(typedDecl)
    }
    val typ = getModuleType(sourceFile.scope, currentEnv)
    val inferredSourceFile = T.SourceFile(
      meta = sourceFile.meta.typed(typ),
      scope = sourceFile.scope,
      declarations = declarations.toArray
    )
    val errors = ListBuffer.empty[Diagnostic]
    inferredSourceFile.copy(
      meta = inferredSourceFile.meta.withDiagnostics(errors)
    )
  }

  private def inferDeclaration(env: Env)(declaration: P.Declaration): (T.Declaration, Env) = {
    declaration match  {
      case l: P.Declaration.Let => inferLetDeclaration(l, env)
      case m: P.Declaration.Module => inferModuleDeclaration(m, env)
      case e: P.Declaration.Error =>
        T.Declaration.Error(e.meta.typed(Value.Uninferred)) -> env
    }
  }

  private def getModuleType(scope: Scope, env: Env): Type = {
    Value.Module(
      scope.symbols.values
        .map(s => s -> env.getTypeOf(s).getOrElse(Uninferred))
        .toMap
    )
  }

  private def inferLetDeclaration(let: P.Declaration.Let, env: Env): (T.Declaration, Env) = {
    let match {
      case P.Declaration.Let(meta, pattern: P.Pattern.Var, Some(rhs)) =>
        val inferredRhs = inferTerm(rhs, env)
        val (inferredPattern, _newEnv) = checkPattern(pattern, inferredRhs.meta.typ, env)
        val newEnv = _newEnv.withValue(
          getPatternSymbol(inferredPattern), toValue(inferredRhs, _newEnv))

        T.Declaration.Let(
          meta.typed(Primitive.Unit),
          inferredPattern,
          Some(inferredRhs)
        ) -> newEnv
      case P.Declaration.Let(meta,  pattern: P.Pattern.Annotated, Some(rhs)) =>
        val (typedPattern, _newEnv) = inferPattern(pattern, env)
        val typedRhs = checkTerm(rhs, typedPattern.meta.typ, env)
        val newEnv = _newEnv.withValue(
          getPatternSymbol(typedPattern),
          toValue(typedRhs, _newEnv)
        )

        T.Declaration.Let(
          meta.typed(Primitive.Unit),
          typedPattern,
          Some(typedRhs)
        ) -> newEnv
    }
  }

  private def inferTerm(term: P.Term, env: Env): T.Term = {
    term match {
      case f: P.Term.Func => inferFunc(f, env)
      case v: P.Term.Var => inferVarTerm(v, env)
      case i: P.Term.If => inferIfTerm(i, env)
      case c: P.Term.Call => inferCall(c, env)
      case t: P.Term.Literal => inferLiteral(t, env)
      case t =>
        utils.todo("Match error: " + t.getClass.getName)
    }
  }

  private def inferLiteral(literal: P.Term.Literal, env: Env): T.Term = {
    literal.variant match {
      case P.Term.Literal.LInt(value) =>
        T.Term.Literal(literal.meta.typed(Primitive.Int), T.Term.Literal.LInt(value))
    }
  }

  private def inferCall(c: P.Term.Call, env: Env): T.Term.Call = {
    val typedFunc = inferTerm(c.func, env)
    val typedArgs = ListBuffer.empty[T.Term.Call.Arg]
    var currentReturnType: Type = typedFunc.meta.typ
    for (arg <- c.args) {
      currentReturnType match {
        case Arrow(from, to) =>
          val typedTerm = checkTerm(arg.value, from, env)
          typedArgs.append(T.Term.Call.Arg(
            arg.meta.typed(typedTerm.meta.typ),
            None, // TODO: Handled named args
            typedTerm
          ))
          currentReturnType = to
        case _ =>
          utils.todo("Extra arg")
      }
    }
    T.Term.Call(
      c.meta.typed(currentReturnType),
      typedFunc,
      typedArgs.toArray
    )
  }

  private def inferIfTerm(t: P.Term.If, env: Env): T.Term.If = {
    val typedPredicate = checkTerm(t.predicate, Primitive.Bool, env)
    val typedTrueBranch = inferTerm(t.trueBranch, env)
    val typedFalseBranch = t.falseBranch.map(
      f => checkTerm(f, typedTrueBranch.meta.typ, env))
    val resultType = typedFalseBranch.map(_ => typedTrueBranch.meta.typ).getOrElse(Primitive.Unit)
    T.Term.If(
      t.meta.typed(resultType),
      typedPredicate,
      typedTrueBranch,
      typedFalseBranch
    )
  }

  private def inferVarTerm(term: P.Term.Var, env: Env): T.Term.Var = {
    val typedIdent = inferIdent(term.ident, env)

    T.Term.Var(
      term.meta.typed(typedIdent.meta.typ),
      typedIdent
    )
  }

  private def inferIdent(value: P.Ident, env: Env): T.Ident = {
    value.scope.resolve(value.name) match {
      case Some(symbol) =>
        env.getTypeOf(symbol) match {
          case Some(t) =>
            T.Ident(
              value.meta.typed(t),
              symbol
            )
          case None => utils.todo(s"CompilerBug: Symbol $value has no type")
        }
      case None => utils.todo(s"Unbound var $value")
    }
  }

  private def inferFunc(f: P.Term.Func, env: Env): T.Term = {
    var newEnv = env
    val typedParams = f.params.map(param => {
      val (p, _newEnv) = inferPattern(param.pattern, newEnv)
      newEnv = _newEnv
      T.Term.Param(p)
    })
    val typedReturnType = f.returnType.map(r => checkTerm(r, Primitive.Type, newEnv))

    val typedBody = typedReturnType match {
      case Some(retTyp) => checkTerm(f.body, toValue(retTyp, newEnv), newEnv)
      case None => inferTerm(f.body, newEnv)
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

  private def inferPattern(pattern: P.Pattern, env: Env): (T.Pattern, Env) = {
    pattern match {
      case P.Pattern.Annotated(meta, innerPattern, typeAnnotation) =>
        val typedAnnotation = checkTerm(typeAnnotation, Primitive.Type, env)
        val (typedPattern, newEnv) = checkPattern(innerPattern, toValue(typedAnnotation, env), env)
        T.Pattern.Annotated(
          meta.typed(typedPattern.meta.typ),
          typedPattern,
          typedAnnotation
        ) -> newEnv
    }
  }

  private def checkPattern(pattern: P.Pattern, expectedType: Type, env: Env): (T.Pattern, Env) = {
    pattern match {
      case P.Pattern.Var(meta, ident) =>
        val typedIdent = checkBindingIdent(ident, expectedType, env)
        T.Pattern.Var(
          meta.typed(expectedType),
          typedIdent
        ) -> env.withType(typedIdent.name, expectedType)
    }
  }

  private def checkBindingIdent(ident: P.Ident, expectedType: Type, env: Env): T.Ident = {
    ident.scope.getSymbol(ident.name) match {
      case Some(s) =>
        ctx.setType(s, expectedType)
        T.Ident(
          ident.meta.typed(expectedType),
          s
        )
      case None => utils.todo(s"Compiler bug: No symbol for $ident")


    }
  }

  private def toValue(term: T.Term, env: Env): Value = {
    term match {
      case T.Term.Var(_, ident) =>
        val result = env.getValueOf(ident.name).getOrElse(Value.Var(ident.name))
        result
      case f: T.Term.Func =>
        f.params.foldRight(toValue(f.body, env))((param, value) => Value.Func(arg =>
          subst(value, getPatternSymbol(param.pattern), arg)))
      case i: T.Term.If =>
        val predicateValue = toValue(i.predicate, env)
        predicateValue match {
          case Value.Bool(true) =>
            val trueBranchValue = toValue(i.trueBranch, env)
            i.falseBranch.map(_ => trueBranchValue).getOrElse(Value.Unit)
          case Value.Bool(false) =>
            i.falseBranch.map(f => toValue(f, env)).getOrElse(Value.Unit)
          case _ =>
            Value.If(
              predicateValue,
              toValue(i.trueBranch, env),
              i.falseBranch.map(f => toValue(f, env)).getOrElse(Value.Unit)
            )
        }
      case c: T.Term.Call =>
        val func = normalize(toValue(c.func, env), env)
        val args = c.args.map(arg => normalize(toValue(arg.value, env), env))
        val result = if (!args.forall(inNormalForm(env))) {
          args.foldLeft(func)(Value.Call.apply)
        } else {
          args.foldLeft(func)((f, arg) => f match {
            case Value.Func(func) =>
              normalize(func(normalize(arg, env)), env)
            case _ => utils.todo(s"Not a function $f")
          })
        }
        println(s"${c.func}(${utils.joinIterable(c.args.map(_.value))}) = $result")
        result
      case T.Term.Literal(_, T.Term.Literal.LInt(value)) =>
        Value.Int(value)
    }
  }

  private def normalize(value: Value, env: Env): Value = {
    value match {
      case c: Constructor => c
      case b: Value.Bool => b
      case i: Value.Int => i
      case Value.Var(s) =>
        env.getValueOf(s).getOrElse(value)
      case Value.If(pred, trueBranch, falseBranch) =>
        normalize(pred, env) match {
          case Value.Bool(true) =>
            normalize(trueBranch, env)
          case Value.Bool(false) =>
            normalize(falseBranch, env)
          case p =>
            Value.If(p, trueBranch, falseBranch)
        }
      case Value.Call(func, arg) =>
        val normalizedFunc = normalize(func, env)
        val normalizedArg = normalize(arg, env)
        normalizedFunc match {
          case Value.Func(f) => normalize(f(normalizedArg), env)
          case _ => Value.Call(normalizedFunc, normalizedArg)
        }
      case f: Value.Func =>
        f
    }
  }

  private def inNormalForm(env: Env)(value: Value): Boolean = {
    value match {
      case v: Value.Var =>
        env.getValueOf(v.symbol) match {
          case Some(other) => inNormalForm(env)(other)
          case None => false
        }
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

  private def checkTerm(term: P.Term, typ: Type, env: Env): T.Term = {
    term match {
      case t =>
        val typedTerm = inferTerm(term, env)
        if (typeEq(typ, typedTerm.meta.typ, env)) {
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

  private def typeEq(expected: Type, found: Type, env: Env): Boolean = {
    (normalize(expected, env), normalize(found, env)) match {
      case (Constructor(s1), Constructor(s2)) => s1.id == s2.id
      case _ =>
        false
    }
  }

  private def inferModuleDeclaration(module: P.Declaration.Module, env: Env): (T.Declaration, Env) = {
    ???
  }

}

