package lmc

import lmc.common.{Loc, ScopeEntry, Symbol, TypeEntry}
import lmc.diagnostics._
import lmc.types._
import lmc.utils.Debug

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable


final class TypeChecker(
  private val ctx: Context.TC,
) {
  import lmc.syntax.{Parsed => P, Typed => T}

  private type E = Diagnostic => Unit

  private val Primitive = ctx.Primitive

  private val _symbolTypes = mutable.HashMap.empty[Symbol, Type]
  private val _typeVars = mutable.HashMap.empty[Symbol, Type]
  private val _symbolKinds = mutable.HashMap.empty[Symbol, Kind]
  private val _checked = mutable.Set.empty[Int]
  private val _generics = mutable.HashMap.empty[Int, Type]

  private var _constraints = mutable.ListBuffer.empty[Constraint]

  def symbolTypes = _symbolTypes
  def generics = _generics

  def addConstraint(constraint: Constraint): Unit = {
    _constraints.append(constraint)
  }

  def constraints: Iterable[Constraint] = _constraints

  def clearConstraints(): Unit = {
    _constraints = ListBuffer.empty
  }

  def inferSourceFile(sourceFile: P.SourceFile): T.SourceFile = {
    val typ = ctx.makeGenericType(sourceFile.loc.path.toString)
    val declarations = sourceFile.declarations.map(d => inferDeclaration(d, None))
    T.SourceFile(
      meta = sourceFile.meta.typed(typ),
      scope = sourceFile.scope,
      declarations = declarations
    )
  }

  private def inferDeclaration(decl: P.Declaration, moduleType: Option[Type]): T.Declaration = {
    decl match {
      case mod: P.Declaration.Module => inferModuleDeclaration(mod)
      case let: P.Declaration.Let => inferLetDeclaration(let, moduleType)
      case alias: P.Declaration.TypeAlias =>
        inferTypeAliasDeclaration(alias, moduleType)
      case e: P.Declaration.Error =>
        T.Declaration.Error(
          e.meta.typed(Primitive.Unit),
          e.modifiers.map(_.typed)
        )
    }
  }

  private def inferTypeAliasDeclaration(
    d: P.Declaration.TypeAlias, moduleType: Option[Type]
  ): T.Declaration.TypeAlias = {
    d match {
      case P.Declaration.TypeAlias(
        meta, modifiers, ident, kindAnnotation, Some(rhs)
      ) =>
        val typedIdent = inferBindingTypeVarIdent(ident)
        val typedKindAnnotation = kindAnnotation.map(inferKindAnnotation)
        val kind = typedKindAnnotation.map(kindAnnotationToKind)
          .getOrElse(Kind.Star)
        val typedRhs = inferTypeAnnotation(rhs)
        addConstraint(HasKind(typedRhs.loc, typedRhs.meta.typ, kind))
        val errors = assignTypeToTVar(ident.loc, typedIdent.name, typedRhs.meta.typ)
        setKindOfSymbol(typedIdent.name, kind)
        None

        T.Declaration.TypeAlias(
          meta.typed(Primitive.Unit).withDiagnostics(errors),
          modifiers.map(_.typed),
          typedIdent,
          typedKindAnnotation,
          Some(typedRhs)
        )
      case P.Declaration.TypeAlias(
        meta, modifiers, ident, kindAnnotation, None
      ) =>
        val typedIdent = inferBindingTypeVarIdent(ident)
        val typedKindAnnotation = kindAnnotation.map(inferKindAnnotation)
        T.Declaration.TypeAlias(
          meta.typed(Primitive.Unit),
          modifiers.map(_.typed),
          typedIdent,
          typedKindAnnotation,
          None
        )
    }
  }

  private def kindAnnotationToKind(
    kindAnnotation: T.KindAnnotation
  ): Kind = {
    kindAnnotation match {
      case T.KindAnnotation.Star(_) => Kind.Star
      case T.KindAnnotation.Func(_, from, to) =>
        from
          .map(kindAnnotationToKind)
          .foldRight(kindAnnotationToKind(to))(
            Kind.KFun.apply
          )
    }
  }

  private def inferKindAnnotation(k: P.KindAnnotation): T.KindAnnotation = {
    k match {
      case P.KindAnnotation.Star(meta) =>
        T.KindAnnotation.Star(meta.typed(Primitive.Unit))
      case P.KindAnnotation.Func(meta, from, to) =>
        T.KindAnnotation.Func(
          meta.typed(Primitive.Unit),
          from.map(inferKindAnnotation),
          inferKindAnnotation(to)
        )
      case P.KindAnnotation.Error(meta) =>
        T.KindAnnotation.Error(
          meta.typed(Primitive.Unit)
        )
    }
  }

  private def inferLetDeclaration(let: P.Declaration.Let, moduleType: Option[Type]): T.Declaration.Let = {
    let match {
      case P.Declaration.Let(meta, modifiers, p: P.Pattern.Var, Some(rhs)) =>
        val typedRhs = inferExpression(rhs)
        val typedPattern = checkPattern(p, typedRhs.meta.typ, moduleType)
        assignExprToPattern(typedRhs, typedPattern)
        T.Declaration.Let(
          meta.typed(Primitive.Unit),
          modifiers.map(_.typed),
          typedPattern,
          Some(typedRhs)
        )
      case P.Declaration.Let(meta, modifiers, p: P.Pattern.Annotated, Some(rhs)) =>
        val typedPattern = inferPattern(p, moduleType)
        val typedRhs = checkExpression(rhs, typedPattern.meta.typ)
        T.Declaration.Let(
          meta.typed(typedRhs.meta.typ),
          modifiers.map(_.typed),
          typedPattern,
          Some(typedRhs)
        )
      case P.Declaration.Let(meta, modifiers, pattern, None) =>
        val typedPattern = inferPattern(pattern, moduleType)
        T.Declaration.Let(
          meta.typed(Primitive.Unit),
          modifiers.map(_.typed),
          typedPattern, None
        )
    }
  }

  private def assignExprToPattern(e: T.Expression, p: T.Pattern): Unit = {
    p.meta.typ match {
      case ExistentialInstance(id, _) =>
        _generics.update(id, e.meta.typ)
      case _ => ()
    }
  }

  private def inferPattern(p: P.Pattern, moduleType: Option[Type]): T.Pattern = {
    println("inferPattern")
    p match {
      case P.Pattern.Var(meta, ident) =>
        println(s"inferPattern(var)(${ident.name})")
        var typedIdent = inferBindingVarIdent(ident)
        typedIdent = typedIdent.copy(
          meta = typedIdent.meta.withDiagnostic(
            Diagnostic(
              loc = typedIdent.meta.loc,
              severity = Severity.Error,
              variant = MissingTypeAnnotation
            )
          )
        )
        T.Pattern.Var(
          meta.typed(typedIdent.meta.typ),
          typedIdent
        )
      case P.Pattern.Annotated(meta, inner, annotation) =>
        val typedAnnotation = inferTypeAnnotation(annotation)
        val typedInner = checkPattern(inner, typedAnnotation.meta.typ, moduleType)
        T.Pattern.Annotated(
          meta.typed(typedAnnotation.meta.typ),
          typedInner,
          typedAnnotation
        )
      case P.Pattern.Error(m) =>
        T.Pattern.Error(m.typed(Uninferred))
    }
  }

  private def inferTypeAnnotation(
    annotation: P.TypeAnnotation,
  ): T.TypeAnnotation = {
    annotation match {
      case P.TypeAnnotation.Var(meta, ident) =>
        val typedIdent = inferTypeVarIdent(ident)
        T.TypeAnnotation.Var(
          meta.typed(typedIdent.meta.typ),
          typedIdent
        )
      case P.TypeAnnotation.Forall(meta, scope, params, body) =>
        val typedParams = inferGenericParams(params)
        val typedBody = inferTypeAnnotation(body)
        val typ = typedParams.map(_.ident.name)
            .foldRight(typedBody.meta.typ)(Forall.apply)
        T.TypeAnnotation.Forall(
          meta.typed(typ),
          scope,
          typedParams,
          typedBody
        )
      case P.TypeAnnotation.Func(meta, funcScope, from, to) =>
        val typedFrom = from.map({
          case (Some(ident), to) =>
            val typedTo = inferTypeAnnotation(to)
            (Some(T.Ident(
              meta.typed(typedTo.meta.typ),
              ctx.makeSymbol(ident.name)
            )), typedTo)
          case (None, to) =>
            (None, inferTypeAnnotation(to))
        })
        val typedTo = inferTypeAnnotation(to)
        val typ = Func(
          typedFrom.map({
            case (label, annot) =>
              label.map(_.name) -> annot.meta.typ
          }),
          typedTo.meta.typ
        )
        T.TypeAnnotation.Func(
          meta.typed(typ),
          funcScope,
          typedFrom,
          typedTo
        )
      case P.TypeAnnotation.Error(meta) =>
        T.TypeAnnotation.Error(meta.typed(Primitive.Unit))
    }
  }

  private def inferGenericParams(params: Array[P.GenericParam]): Array[T.GenericParam] = {
    params.map(inferGenericParam)
  }

  private def inferGenericParam(param: P.GenericParam): T.GenericParam = {
    val ident = inferBindingTypeVarIdent(param.ident)
    val kindAnnotation = param.kindAnnotation.map(inferKindAnnotation)
    val kind = kindAnnotation.map(kindAnnotationToKind).getOrElse(Kind.Star)
    setKindOfSymbol(ident.name, kind)
    T.GenericParam(
      param.meta.typed(Primitive.Unit),
      ident,
      kindAnnotation
    )
  }



  private def checkTypeAnnotation(annotation: P.TypeAnnotation, t: Type): T.TypeAnnotation = {
    val inferred = inferTypeAnnotation(annotation)
    addConstraint(Unifies(annotation.loc, t, inferred.meta.typ))
    inferred
  }

  private def inferTypeVarIdent(ident: P.Ident): T.Ident = {
    ident.scope.resolveTypeEntry(ident.name) match {
      case Some(TypeEntry(symbol)) =>
        val typ = resolveTypeVar(symbol)
        T.Ident(
          ident.meta.typed(typ),
          symbol
        )
      case None =>
        T.Ident(
          ident.meta.typed(Uninferred).withDiagnostic(
            Diagnostic(
              loc = ident.loc,
              severity = Severity.Error,
              variant = UnBoundTypeVar(ident.name)
            )
          ),
          ctx.makeSymbol(ident.name)
        )
    }
  }

  private def resolveTypeVar(symbol: Symbol): Type = {
    _typeVars.get(symbol) match {
      case Some(Var(sym)) =>
        resolveTypeVar(sym)
      case Some(t) => t
      case None => Var(symbol)
    }
  }

  private def checkExpression(e: P.Expression, typ: Type): T.Expression = {
    e match {
      case f: P.Expression.Func =>
        checkFunc(f,typ)
      case _ =>
        val typed = inferExpression(e)
        addConstraint(Unifies(e.loc, typ, typed.meta.typ))
        typed
    }
  }
  private def inferExpression(e: P.Expression): T.Expression = e match {
    case P.Expression.Literal(meta, P.Expression.Literal.LInt(x)) =>
      T.Expression.Literal(meta.typed(Primitive.Int), T.Expression.Literal.LInt(x))
    case P.Expression.Prop(meta, expr, prop) =>
      val typedExpr = inferExpression(expr)
      val typedProp = inferProp(typedExpr, prop)
      addConstraint(HasProperty(typedProp.loc, typedExpr.meta.typ, prop.name, typedProp.meta.typ))
      T.Expression.Prop(
        meta.typed(typedProp.meta.typ),
        typedExpr,
        typedProp
      )
    case P.Expression.Var(meta, ident) =>
      val typedIdent = inferVarIdent(ident)
      T.Expression.Var(
        meta.typed(typedIdent.meta.typ),
        typedIdent
      )
    case c: P.Expression.Call =>
      inferCall(c)
    case f:P.Expression.Func =>
      inferFunc(f)
  }

  private def inferProp(lhs: T.Expression, rhs: P.Ident): T.Ident = {
    lhs match {
      case T.Expression.Var(_, ident) =>
        ident.name.members.get(rhs.name) match {
          case Some(propSymbol) =>
            val typ = getTypeOfSymbol(propSymbol) match {
              case Some(t) => t
              case None => ctx.makeGenericType(propSymbol.text)
            }
            T.Ident(
              rhs.meta.typed(typ),
              propSymbol
            )
          case None =>
            ???
        }
      case T.Expression.Prop(_, _, p) =>
        println(p.name.members, rhs.name)
        p.name.members.get(rhs.name) match {
          case Some(propSymbol) =>
            val typ = getTypeOfSymbol(propSymbol) match {
              case Some(t) => t
              case None =>
                ctx.makeGenericType(propSymbol.text)
            }
            T.Ident(
              rhs.meta.typed(typ),
              propSymbol
            )
          case None =>
            ???
        }
    }
  }

  private def inferCall(call: P.Expression.Call): T.Expression = {
    val func = inferExpression(call.func)
    addConstraint(IsFunction(func.meta.loc, func.meta.typ))
    val args = call.args.zipWithIndex.map({
      case (P.Expression.Call.Arg(meta, label, value), index) =>
        val typedValue = inferExpression(value)
        val typedLabel = label.map(l => inferArgLabelIdent(l, typedValue.meta.typ))
        addConstraint(typedLabel match {
          case Some(l) =>
            TakesLabeledArg(
              typedValue.loc,
              func.meta.typ,
              label = (l.loc, l.name.text),
              typedValue.meta.typ
            )
          case None =>
            TakesPositionalArg(
              typedValue.loc,
              func.meta.typ,
              index = index,
              typedValue.meta.typ
            )
        })
        T.Expression.Call.Arg(
          meta.typed(typedValue.meta.typ),
          typedLabel,
          typedValue
        )
    })
    addConstraint(FunctionApplication(func.loc, func.meta.typ, args.map(arg => {
      (arg.loc, arg.label.map(p => p.name.text -> p.loc), arg.meta.typ)
    })))
    val returnType = ctx.makeGenericType("T")
    addConstraint(
      HasReturnType(loc = call.loc, func.meta.typ, returnType)
    )
    T.Expression.Call(
      call.meta.typed(returnType),
      func,
      args
    )
  }

  private def inferArgLabelIdent(ident: P.Ident, typ: Type): T.Ident = {
    val symbol = ctx.makeSymbol(ident.name)
    T.Ident(
      ident.meta.typed(typ),
      symbol
    )
  }

  private def checkFunc(func: P.Expression.Func, typ: Type): T.Expression = {
    typ match {
      case Func(from, to) =>
        val funcParamsVec = func.params.toVector
        var i = 0
        val checkedParams = ListBuffer.empty[T.Expression.Param]
        for ((expectedParamLabel, expectedParamTyp) <- from) {
          if (i < funcParamsVec.length) {
            val funcParam = funcParamsVec(i)
            var checkedPattern = checkPattern(
              funcParam.pattern, expectedParamTyp, moduleType = None
            )
            expectedParamLabel match {
              case Some(expectedLabelSym) =>
                val foundLabel = getPatternLabel(checkedPattern)
                if (!foundLabel.map(_.name.text).contains(expectedLabelSym.text)) {
                  checkedPattern = checkedPattern.withMeta(
                    meta = checkedPattern.meta.withDiagnostic(
                      Diagnostic(
                        loc = foundLabel.map(_.loc)
                          .getOrElse(checkedPattern.loc),
                        severity = Severity.Error,
                        variant = FuncParamLabelMismatch(expectedLabelSym.text)
                      )
                    )
                  )
                }
              case None =>
                ()
            }
            checkedParams.append(T.Expression.Param(checkedPattern))
          }
          i += 1
        }
        while (i < funcParamsVec.length) {
          val funcParam = funcParamsVec(i)
          var inferredPattern = inferPattern(funcParam.pattern, moduleType = None)
          inferredPattern = inferredPattern.withMeta(
            meta = inferredPattern.meta.withDiagnostic(
              Diagnostic(
                loc = inferredPattern.loc,
                severity = Severity.Error,
                variant = ExtraParam
              )
            )
          )
          checkedParams.append(T.Expression.Param(inferredPattern))
          i += 1
        }
        val checkedRetTypAnnotation = func
          .returnType
          .map({ checkTypeAnnotation(_, to) })
        val checkedBody = checkedRetTypAnnotation match {
          case Some(annot) =>
            val t = annot.meta.typ
            checkExpression(func.body, t)
          case None =>
            checkExpression(func.body, to)
        }
        val checkedGenericParams = inferGenericParams(func.genericParams)
        T.Expression.Func(
          func.meta.typed(typ),
          func.fnToken,
          func.funcScope,
          checkedGenericParams,
          checkedParams.toArray,
          checkedRetTypAnnotation,
          checkedBody
        )
      case _ =>
        val e = inferExpression(func)
        addConstraint(Unifies(e.loc, typ, e.meta.typ))
        e
    }
  }

  private def inferFunc(func: P.Expression.Func): T.Expression = {
    Debug.log(s"inferFunc($func)")
    var inferringPositional = true
    val inferredGenericParams = inferGenericParams(func.genericParams)
    val inferredParams = func.params.map(param => {
      var inferredPattern = inferPattern(param.pattern, None)
      getPatternLabel(inferredPattern) match {
        case Some(_) =>
          inferringPositional = false
        case None =>
          if (inferringPositional) {
            inferredPattern = inferredPattern.withMeta(
              meta = inferredPattern.meta.withDiagnostic(
                Diagnostic(
                  loc = param.pattern.loc,
                  severity = Severity.Error,
                  variant = PositionalParamAfterLabelled
                )
              )
            )
          }
      }
      T.Expression.Param(inferredPattern)
    })
    val (checkedBody, checkedReturnTypeAnnotation) = func.returnType match {
      case Some(t) =>
        val checkedRetTypAnnotation = inferTypeAnnotation(t)
        addConstraint(HasKind(t.loc, checkedRetTypAnnotation.meta.typ, Kind.Star))
        val expectedRetTyp = checkedRetTypAnnotation.meta.typ
        (checkExpression(func.body, expectedRetTyp), Some(checkedRetTypAnnotation))
      case None =>
        (inferExpression(func.body), None)
    }
    val from = inferredParams.map(param => {
      (getPatternLabel(param.pattern).map(_.name), param.pattern.meta.typ)
    })
    val innerTyp = Func(from, checkedBody.meta.typ)
    val typ = if (func.genericParams.isEmpty) {
      innerTyp
    } else {
      makeForall(inferredGenericParams.map(_.ident.name), innerTyp)
    }
    T.Expression.Func(
      func.meta.typed(typ),
      func.fnToken,
      func.scope,
      inferredGenericParams,
      inferredParams,
      checkedReturnTypeAnnotation,
      checkedBody
    )
  }

  private def getPatternLabel(pattern: T.Pattern): Option[T.Ident] = {
    pattern match {
      case T.Pattern.Var(_, ident) => Some(ident)
      case T.Pattern.Annotated(_, p, _) =>
        getPatternLabel(p)
      case T.Pattern.Paren(_, p) => getPatternLabel(p)
      case _ => None
    }
  }

  private def makeForall(paramNames: Iterable[Symbol], resultType: Type): Type = {
    paramNames.foldRight(resultType)((current, prev) => Forall(current, prev))
  }

  private def inferVarIdent(ident: P.Ident): T.Ident = {
    ident.scope.resolveSymbol(ident.name) match {
      case Some(symbol) =>
        val typ = getTypeOfSymbol(symbol) match {
          case Some(t) =>
            t
          case None =>
            ctx.makeGenericType(symbol.text)
        }

        val error = ctx.getDeclOf(symbol) match {
          case Some(d: P.Declaration.Let) =>
            if (d.loc.start > ident.loc.start) {
              Some(
                Diagnostic(
                  loc = ident.loc,
                  severity = Severity.Error,
                  variant = UseBeforeAssignment(symbol.text)
                )
              )
            } else {
              None
            }
          case Some(_) =>
            None
          case None => None
        }
        setTypeOfSymbol(symbol, typ)
        T.Ident(
          ident.meta.typed(typ).withDiagnostics(error),
          symbol
        )
      case None =>
        val symbol = ctx.makeSymbol(ident.name)
        T.Ident(
          ident.meta.typed(Uninferred).withDiagnostic(
            Diagnostic(
              loc = ident.loc,
              severity = Severity.Error,
              variant = UnBoundVar(ident.name)
            )
          ),
          symbol
        )
    }
  }

  private def checkPattern(p: P.Pattern, typ: Type, moduleType: Option[Type]): T.Pattern = p match {
    case P.Pattern.Var(meta, ident) =>
      T.Pattern.Var(
        meta.typed(typ),
        checkBindingVarIdent(ident, typ, moduleType)
      )
    case P.Pattern.Paren(meta, inner) =>
      val typedInner = checkPattern(inner, typ, moduleType)
      T.Pattern.Paren(
        meta.typed(typ),
        typedInner
      )
    case P.Pattern.Annotated(meta, inner, annotation) =>
      val typedAnnotation = checkTypeAnnotation(annotation, typ)
      val typedInner = checkPattern(
        inner,
        typedAnnotation.meta.typ,
        moduleType)
      T.Pattern.Annotated(
        meta.typed(typedAnnotation.meta.typ),
        typedInner,
        typedAnnotation
      )
  }

  private def checkBindingVarIdent(ident: P.Ident, t: Type, moduleType: Option[Type]): T.Ident = {
    moduleType match {
      case Some(modTyp) =>
        addConstraint(HasDeclaration(ident.loc, modTyp, ident.name, t))
      case None =>
        ()
    }
    ident.scope.symbols.get(ident.name) match {
      case Some(ScopeEntry(symbol, _)) =>
        getTypeOfSymbol(symbol) match {
          case Some(ExistentialInstance(id, _)) =>
            _generics.update(id, t)
          case None => ()
        }
        setTypeOfSymbol(symbol, t)
        T.Ident(
          ident.meta.typed(t),
          symbol
        )
      case None =>
        val symbol = ctx.makeSymbol(ident.name)
        setTypeOfSymbol(symbol, t)
        T.Ident(
          ident.meta.typed(t),
          symbol
        )
    }
  }

  private def inferModuleDeclaration(module: P.Declaration.Module): T.Declaration = {
    val ident = inferBindingVarIdent(module.ident)
    val declarations = module.body.map(d => inferDeclaration(d, Some(ident.meta.typ)))
    val modTyp = Module(Map.empty, module.moduleScope.symbols.map({
      case (k, entry) =>
        val typ = getTypeOfSymbol(entry.symbol).getOrElse({
          val t = ctx.makeGenericType(entry.symbol.text)
          setTypeOfSymbol(entry.symbol, t)
          t
        })
        entry.symbol -> typ
    }).toMap)
//    addConstraint(Unifies(ident.loc, ident.meta.typ, modTyp))
//    val genericIdents = inferGenericIdents()
    T.Declaration.Module(
      meta = module.meta.typed(ident.meta.typ),
      module.modifiers.map(_.typed),
      ident,
      module.scope,
      Array(),
      body = declarations
    )
  }


  private def inferBindingVarIdent(ident: P.Ident): T.Ident = {
    println(s"inferBindingVarIdent(${ident.name})")
    val symbol = ident.scope.getSymbol(ident.name).get
    println(symbol)
    if (ident.name == "z1") {
      Debug.start()
    }
    val typ = getTypeOfSymbol(symbol) match {
      case Some(t) =>
        Debug.log(t)
        t
      case None =>
        Debug.log("None")
        ctx.makeGenericType(ident.name)
    }
    setTypeOfSymbol(symbol, typ)
    T.Ident(
      meta = ident.meta.typed(typ),
      name = symbol
    )
  }

  def applyEnv(t: Type): Type = {
    t match {
      case e@ExistentialInstance(i, n) =>
        _generics.get(i) match {
          case Some(ty) =>
            if (ty != t) {
              applyEnv(ty)
            } else {
              t
            }
          case None => t
        }
      case v@Var(symbol) =>
        getTypeVar(symbol).map(applyEnv).getOrElse(v)
      case Module(types, values) =>
        Module(types, values.mapValues(applyEnv))
      case _ => t
    }
  }

  def inferBindingTypeVarIdent(ident: P.Ident): T.Ident = {
    ident.scope.resolveTypeEntry(ident.name) match {
      case Some(TypeEntry(symbol)) =>
        T.Ident(
          ident.meta.typed(Primitive.Unit),
          symbol
        )
      case None => ???
    }
  }

  def getTypeOfSymbol(symbol: Symbol): Option[Type] = {
    _symbolTypes.get(symbol)
  }


  def setTypeOfSymbol(symbol: Symbol, typ: Type): Unit = {
    _symbolTypes.get(symbol) match {
      case Some(e: ExistentialInstance) =>
        _generics.update(e.id, typ)
        for ((k, v) <- _symbolTypes) {
          def go(t: Type): Unit = {
            t match {
              case ExistentialInstance(id, name) if e.id == id =>
                _symbolTypes.update(k, typ)
              case Module(_, values) =>
                values.values.foreach(go)
              case _ => ()
            }
          }
          go(v)
        }
      case Some(_) =>
        ()
      case None =>
        _symbolTypes.update(symbol, typ)
    }
  }

  def assignTypeToTVar(loc: Loc, symbol: Symbol, typ: Type): Iterable[Diagnostic] = {
    if (occursIn(symbol, typ)) {
      Some(
        Diagnostic(
          loc = loc,
          severity = Severity.Error,
          variant = CyclicType
        )
      )
    } else {
      _typeVars.update(symbol, typ)
      None
    }
  }

  private def occursIn(symbol: Symbol, typ: Type): Boolean = {
    typ match {
      case Var(s) if s == symbol => true
      case Var(_) => false
      case Constructor(_, _) | Uninferred | ErrorType => false
      case Func(from, to) =>
        from.map(_._2).exists(t => occursIn(symbol, t)) ||
          occursIn(symbol, to)
      case TApplication(f, arg) =>
        occursIn(symbol, f) || occursIn(symbol, arg)
      case Module(_, values) =>
        values.values.exists(t => occursIn(symbol, t))
      case Forall(_, t) => occursIn(symbol, t)
    }
  }

  def setTypeVar(symbol: Symbol, typ: Type): Unit = {
    _typeVars .update(symbol, typ)
  }

  def getTypeVar(symbol: Symbol): Option[Type] = {
    _typeVars.get(symbol)
  }

  def getKindOfSymbol(symbol: Symbol): Option[Kind] = {
    _symbolKinds.get(symbol)
  }

  def setKindOfSymbol(symbol: Symbol, kind: Kind): Unit = {
    _symbolKinds.update(symbol, kind)
  }
}

sealed trait Constraint {
  def loc: Loc

  override def toString: String = this match {
    case Unifies(_, expected, found) => s"$expected == $found"
    case HasDeclaration(_, t, prop, propType) => s"let $t.$prop: $propType"
    case HasProperty(_, t, prop, propType) => s"$t.$prop == $propType"
    case HasKind(_, t, kind) => s"$t : $kind"
    case TakesPositionalArg(_, t, i, argType) => s"$t takes positional arg $i: $argType"
    case TakesLabeledArg(_, t, (_, label), argType) =>
      s"$t takes arg ($label: $argType)"
    case IsFunction(_, t) => s"$t is a function"
    case HasReturnType(_, f, r) => s"$f: fn(..) => $r"
  }
}
case class Unifies(loc: Loc, expected: Type, found: Type) extends Constraint
case class HasProperty(loc: Loc, t: Type, prop: String, propType: Type) extends Constraint
case class HasDeclaration(loc: Loc, t: Type, prop: String, propType: Type) extends Constraint
case class HasKind(loc: Loc, t: Type, kind: Kind) extends Constraint
case class TakesLabeledArg(
  loc: Loc,
  t: Type,
  label: (Loc, String),
  paramTyp: Type
) extends Constraint
case class TakesPositionalArg(
  loc: Loc,
  t: Type,
  index: Int,
  argType: Type
) extends Constraint
case class HasReturnType(
  loc: Loc,
  t: Type,
  returnType: Type
) extends Constraint
case class IsFunction(loc: Loc, typ: Type) extends Constraint
case class FunctionApplication(
  loc: Loc,
  func: Type,
  args: Array[(Loc, Option[(String, Loc)], Type)]
) extends Constraint

