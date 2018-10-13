package lmc

import jdk.jshell.Diag
import lmc.diagnostics._
import lmc.Value.{Arrow, Constructor, ModuleType, TaggedUnion, Type, TypeOf, Uninferred}
import lmc.common.{HasLoc, Scope, Symbol}

import scala.annotation.tailrec
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
  private var _nextGeneric: Int = 0

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
      case e: P.Declaration.Enum => inferEnumDecl(e)
      case e: P.Declaration.Error =>
        T.Declaration.Error(e.meta.typed(Value.Uninferred))
    }
  }

  private def inferEnumDecl(e: P.Declaration.Enum): T.Declaration.Enum = {
    val symbol = e.scope.getSymbol(e.ident.name).get
    val declType: Type = Value.Constructor(symbol)

    val recursiveSymbol = e.scope.getSymbol(e.ident.name).get
    val recursiveSymbolType = Primitive.Type
    val recursiveSymbolValue = declType

    setTypeOfSymbol(recursiveSymbol, recursiveSymbolType)
    setValueOfSymbol(recursiveSymbol, recursiveSymbolValue)

    val cases = e.cases.map(inferEnumCase(declType))

    val enumType = Value.Constructor(symbol)

    val typedIdent = checkBindingIdent(e.ident, Primitive.Type)
    setValueOfSymbol(typedIdent.name, enumType)

    T.Declaration.Enum(
      e.meta.typed(enumType),
      typedIdent,
      cases
    )
  }

  private def inferEnumCase(enumType: Type)(c: P.EnumCase): T.EnumCase = {
    val params = c.params.map(inferBinder)
    val typ = params.map(_.meta.typ).foldRight(enumType)(Value.Arrow.apply)
    val ident = checkBindingIdent(c.ident, typ)
    val returnValue: Value = TaggedUnion(ident.name, params.map(_.name.name).map({ Value.Var }))
    val value = params.foldRight(returnValue)((binder, ret) => Value.Func(value => {
      subst(ret, binder.name.name, value)
    }))
    setValueOfSymbol(ident.name, value)
    T.EnumCase(
      c.meta.typed(typ),
      c.caseScope,
      ident,
      params
    )

  }

  private def getModuleType(scope: Scope): Type = {
    Value.Module(
      scope.symbols.values
        .map(s => s -> getTypeOfSymbol(s))
        .toMap
    )
  }

  private def inferLetDeclaration(let: P.Declaration.Let): T.Declaration = {
    val result = let match {
      case P.Declaration.Let(meta, binder@P.Binder(_, _, None), Some(rhs)) =>
        val inferredRhs = inferTerm(rhs)
        val typedBinder = checkBinder(binder, inferredRhs.meta.typ)
        setValueOfSymbol(
          typedBinder.name.name,
          toValue(inferredRhs)
        )

        T.Declaration.Let(
          meta.typed(Primitive.Unit),
          typedBinder,
          Some(inferredRhs)
        )
      case P.Declaration.Let(meta,  binder@P.Binder(_, _, Some(annotation)), Some(rhs)) =>
        val typedBinder = inferBinder(binder)
        val typedRhs = checkTerm(rhs, typedBinder.meta.typ)

        setValueOfSymbol(
          typedBinder.name.name,
          toValue(typedRhs)
        )

        T.Declaration.Let(
          meta.typed(Primitive.Unit),
          typedBinder,
          Some(typedRhs)
        )
    }
    result
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
          case b: P.Term.Block => inferBlock(b)
        }
        _inferredTerms.update(term, t)
        t
    }
  }

  private def inferBlock(block: P.Term.Block): T.Term.Block = {
    val typedMembers = block.members.map({
      case t: P.Term => inferTerm(t)
      case d: P.Declaration => inferDeclaration(d)
    })
    val typ = typedMembers.lastOption match {
      case Some(t: T.Term) => t.meta.typ
      case Some(_: T.Declaration) => Primitive.Unit
      case None => Primitive.Unit
    }
    T.Term.Block(
      block.meta.typed(typ),
      block.blockScope,
      typedMembers
    )
  }

  private def inferProp(prop: P.Term.Prop): T.Term = {
    val typedLhs = inferTerm(prop.expr)
    val typ = Value.TypeOfModuleMember(toValue(typedLhs), prop.prop.name)
    val typedRhs = T.Ident(
      prop.prop.meta.typed(typ),
      ctx.makeSymbol(prop.prop.name, P.Declaration.Error(prop.meta))
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
      case s if !isReducible(funcType) =>
        utils.todo(s"$s is not a function, $arg")
      case _ =>
        checkFunctionArg(arg, reduce(funcType))
    }
  }

  private def reduce(value: Value): Value = {
    import Value._
    value match {
      case Var(s) if getValueOfSymbol(s).isDefined =>
        getValueOfSymbol(s).get
      case TypeOf(symbol) => getTypeOfSymbol(symbol)
      case If(Bool(true), t, _) => t
      case If(Bool(false), _, f) => f
      case If(p, t, f) if isReducible(p) => Value.If(reduce(p), t, f)
      case Call(Func(f), arg) => f(arg)
      case Call(f, arg) if isReducible(f) => Value.Call(reduce(f), arg)
      case TypeOfModuleMember(m: Module, rhs) if m.symbolMap.contains(rhs) =>
        TypeOf(m.symbolMap(rhs))

      case TypeOfModuleMember(lhs, rhs) =>
        TypeOfModuleMember(reduce(lhs), rhs)
      case Prop(m: Module, rhs) if m.symbolMap.contains(rhs) =>
        m.map(m.symbolMap(rhs))
      case Prop(lhs, rhs) if isReducible(lhs) => Prop(reduce(lhs), rhs)
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
      case None =>
        val symbol = ctx.makeErrorSymbol(value.name)
        ctx.addError(Diagnostic(
          loc = value.loc,
          severity = Severity.Error,
          variant = diagnostics.UnBoundVar(value.name)
        ))
        T.Ident(
          value.meta.typed(Value.Uninferred),
          symbol
        )
    }
  }


  private def inferFunc(f: P.Term.Func): T.Term = {
    val typedParams = f.params.map(inferBinder)
    val typedReturnType = f.returnType.map(r => checkTerm(r, Primitive.Type))

    val typedBody = typedReturnType match {
      case Some(retTyp) => checkTerm(f.body, toValue(retTyp))
      case None => inferTerm(f.body)
    }
    val typ = typedParams.map(_.meta.typ).foldRight(typedBody.meta.typ)(Value.Arrow.apply)
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

  private def inferBinder(binder: P.Binder): T.Binder = {
    binder match {
      case P.Binder(meta, innerPattern, Some(annotation)) =>
        val typedAnnotation = checkTerm(annotation, Primitive.Type)
        val typedIdent = checkBindingIdent(innerPattern, toValue(typedAnnotation))
        T.Binder(
          meta.typed(typedIdent.meta.typ),
          typedIdent,
          Some(typedAnnotation)
        )
    }
  }

  private def checkBinder(binder: P.Binder, expectedType: Type): T.Binder = {
      val typedIdent = checkBindingIdent(binder.name, expectedType)
      T.Binder(
        binder.meta.typed(expectedType),
        typedIdent,
        binder.annotation.map({ inferTerm })
      )
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
          subst(value, param.name.name, arg)))
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
        args.foldLeft(func)(Value.Call.apply)
      case T.Term.Literal(_, T.Term.Literal.LInt(value)) =>
        Value.Int(value)
      case p: T.Term.Prop =>
        Value.Prop(toValue(p.expr), p.prop.name.text)
      case b: T.Term.Block =>
        if (b.members.length == 0) {
          Value.Unit
        } else {
          val lastValueOpt: Option[Value] = b.members.lastOption.flatMap({
            case t: T.Term => Some(toValue(t))
            case _ => None
          })
          val lastValue = lastValueOpt
            .getOrElse(Value.Unit)
          b.members.take(b.members.length - 1).foldRight(lastValue)((current, result) => current match {
            case l: T.Declaration.Let => Value.Let(l.binder.name.name, toValue(l.rhs.get), result)
            case t: T.Term => Value.Let(ctx.makeErrorSymbol("_"), toValue(t), result)
          })
        }
    }
  }

  private def normalize(value: Value, count: Int = 0): Value = {
    if (count > 100) {
      utils.todo("Overflow")
    }

    def go(value: Value): Value = normalize(value, count + 1)
    value match {
      case c: Constructor => c
      case b: Value.Bool => b
      case i: Value.Int => i
      case Value.Var(s) => (for {
        v <- getValueOfSymbol(s)
        _ = setTypeOfSymbol(s, v)
        t = go(v)
        _ = setTypeOfSymbol(s, t)
      } yield t).getOrElse(value)
      case Value.If(pred, trueBranch, falseBranch) =>
        go(pred) match {
          case Value.Bool(true) =>
            go(trueBranch)
          case Value.Bool(false) =>
            go(falseBranch)
          case p =>
            Value.If(p, trueBranch, falseBranch)
        }
      case Value.Call(func, arg) =>
        val normalizedFunc = go(func)
        val normalizedArg = go(arg)
        normalizedFunc match {
          case Value.Func(f) => go(f(normalizedArg))
          case _ => Value.Call(normalizedFunc, normalizedArg)
        }
      case Value.TypeOf(symbol) =>
        val result = go(getTypeOfSymbol(symbol))
        setTypeOfSymbol(symbol, result)
        result
      case Value.ModuleType(m) => Value.ModuleType(m.mapValues(go))
      case Value.TypeOfModuleMember(lhs, rhs) =>
        resolvePropertySymbol(lhs, rhs) match {
          case Some(sym) => getNormalizedType(sym)
          case None =>
            utils.todo(s"$lhs has no property named $rhs")
        }
      case Value.Module(map) =>
        Value.Module(map.mapValues(go))
      case Value.Arrow(from, to) => Value.Arrow(go(from), go(to))

      case f: Value.Func =>
        f
      case v if !isReducible(v) => v
      case v => go(reduce(v))
    }
  }

  @tailrec
  private def resolvePropertySymbol(value: Value, str: String): Option[Symbol] = {
    value match {
      case Value.Module(map) => map.keys.find(_.text == str)
//      case Value.TypeOfModuleMember(lhs, rhs) =>
//        resolvePropertySymbol(lhs, rhs) match {
//          case Some(s) => resolvePropertySymbol(getValueOfSymbol(s).get, str)
//        }
      case v if isReducible(v) => resolvePropertySymbol(reduce(v), str)
    }
  }

  private def inNormalForm(value: Value): Boolean = {
    value match {
      case _: Value.TypeOf => false
      case _: Value.Bool => true
      case _: Value.Int => true
      case _ => !isReducible(value)
    }
  }

  private def subst(value: Value, symbol: Symbol, arg: Value): Value = {
    def go(value: Value): Value = {
      subst(value, symbol, arg)
    }
    value match {
      case Value.Var(s) if s.id == symbol.id => arg
      case Value.Var(_) => value
      case c: Value.Constructor => c
      case Value.Uninferred => value
      case Value.Call(c, arg) =>
        Value.Call(go(c), go(arg))
      case Value.If(pred, t, f) =>
        Value.If(go(pred), go(t), go(f))
      case f: Value.Func => f
      case b: Value.Bool => b
      case i: Value.Int => i
      case t: Value.TaggedUnion =>
        Value.TaggedUnion(t.tag, t.values.map(go))
      case Value.Module(map) =>
        Value.Module(map.mapValues(go))
    }

  }

  private def checkTerm(term: P.Term, typ: Type): T.Term = {
    term match {
      case _ =>
        val typedTerm = inferTerm(term)
        if (!typeEq(typ, typedTerm.meta.typ)) {
          ctx.addError(Diagnostic(
            loc = term.loc,
            severity = Severity.Error,
            variant = TypeMismatch(normalize(typ), normalize(typedTerm.meta.typ))
          ))
        }
        typedTerm
    }
  }

  private def typeEq(expected: Type, found: Type): Boolean = {
    (expected, found) match {
      case (Constructor(s1), Constructor(s2)) => s1.id == s2.id
//      case (Primitive.Type, _: Value.EnumType) => true
      case (Uninferred, _) => true
      case (_, Uninferred) => true
      case _ =>
        if (isReducible(expected)) {
          typeEq(reduce(expected), found)
        } else if (isReducible(found)) {
          typeEq(expected, reduce(found))
        } else {
          false
        }
    }
  }

  private def isReducible(typ: Type): Boolean = {
    import Value._
    typ match {
      case Var(s) if getValueOfSymbol(s).isDefined => true
      case Var(_) => false
      case Uninferred => false
      case i: Value.If if isReducible(i.predicate) => true
      case If(_: Bool, _, _) => true
      case _: Constructor => false
      case TypeOf(s) if _types.contains(s) => true
      case TypeOf(s) => false
      case Call(Value.Func(_), _) => true
      case Call(f, _) if isReducible(f) => true
      case Call(_, arg) if isReducible(arg) => true
      case _: Call => false
      case _: Bool => false
      case _: Int => false
      case ModuleType(members) if members.values.exists(isReducible) => true
      case ModuleType(_) => false
      case Module(members) if members.values.exists(isReducible) => true
      case Module(_) => false
      case TypeOfModuleMember(m: Module, rhs) if m.symbolMap.contains(rhs) => true
      case TypeOfModuleMember(lhs, _) if isReducible(lhs) => true
      case a: Arrow if isReducible(a.from) => true
      case a: Arrow if isReducible(a.to) => true
      case _: Arrow => false
      case _: Func => false
      case _: TaggedUnion => false

      case Prop(m: Module, rhs) if m.symbolMap.contains(rhs) => true
      case p: Value.Prop if isReducible(p.lhs) => true
      case v =>
        utils.todo(s"unmatched: $v of type ${v.getClass.getName}")
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

  private def makeGeneric(): Unit = {
    val id = _nextGeneric
    _nextGeneric += 1
    Value.GenericInstance(id)
  }

}
