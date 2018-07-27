package lmc

import lmc.common.{Loc, Scope, Symbol}
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
  private val _members = mutable.HashMap.empty[Int, mutable.HashMap[String, Type]]

  def symbolTypes = _symbolTypes
  def generics = _generics

  def inferSourceFile(sourceFile: P.SourceFile): T.SourceFile = {
    assignHoles(sourceFile)
    val symbol = ctx.makeSymbol("SourceFile")
    val declarations = sourceFile.declarations.map(inferDeclaration)
    val typ = getModuleType(sourceFile.scope)
    val inferredSourceFile = T.SourceFile(
      meta = sourceFile.meta.typed(typ),
      scope = sourceFile.scope,
      declarations = declarations
    )
    val errors = ListBuffer.empty[Diagnostic]
    getErrors(errors, inferredSourceFile)
    inferredSourceFile.copy(
      meta = inferredSourceFile.meta.withDiagnostics(errors)
    )
  }

  private def inferDeclaration(declaration: P.Declaration): T.Declaration = {
    declaration match  {
      case l: P.Declaration.Let => inferLetDeclaration(l)
      case m: P.Declaration.Module => inferModuleDeclaration(m)
      case d: P.Declaration.TypeAlias => inferTypeAlias(d)
    }
  }

  private def inferModuleDeclaration(m: P.Declaration.Module): T.Declaration = {
    val inferredDeclarations = m.body.map(inferDeclaration)
    val t = getModuleType(m.moduleScope)
    T.Declaration.Module(
      m.meta.typed(t),
      m.modifiers.map(_.typed),
      checkBindingVarIdent(t)(m.ident),
      m.moduleScope,
      inferGenericParams(m.genericParams),
      inferredDeclarations
    )
  }

  private def checkBindingVarIdent(typ: Type)(ident: P.Ident): T.Ident = {
    ident.scope.typeMap.get(ident.name) match {
      case Some((forwardType, symbol)) =>
        val errors = ListBuffer.empty[Diagnostic]
        assertAssignability(errors, ident.loc)(typ, forwardType)
        val t = applyEnv(forwardType, Some(errors))
        setTypeOfSymbol(symbol, t)
        ident.scope.setType(ident.name, symbol, t)
        T.Ident(ident.meta.typed(t), symbol)
      case None =>
        // compiler bug
        ???
    }
  }

  private def inferGenericParams(params: Array[P.GenericParam]): Array[T.GenericParam] = {
    params.map(inferGenericParam)
  }

  private def inferGenericParam(param: P.GenericParam): T.GenericParam = {
    ???
  }



  private def inferTypeAnnotation(annotation: P.TypeAnnotation): T.TypeAnnotation = {
    annotation match {
      case P.TypeAnnotation.Var(meta, ident) =>
        val typedIdent = inferIdent(ident, isType = true)
        T.TypeAnnotation.Var(
          meta.typed(typedIdent.meta.typ),
          typedIdent
        )
      case P.TypeAnnotation.Func(meta, scope, label, from, to) =>
        val typedFrom = inferTypeAnnotation(from)
        val typedTo = inferTypeAnnotation(to)
        val typedLabel = label.map(ident => {
          val name = ctx.makeSymbol(ident.name)
          T.Ident(
            meta = ident.meta.typed(evalTypeAnnotation(typedFrom)),
            name
          )
        })
        T.TypeAnnotation.Func(
          meta.typed(Star),
          scope,
          typedLabel,
          typedFrom,
          typedTo
        )
      case P.TypeAnnotation.Paren(meta, inner) =>
        val typedInner = inferTypeAnnotation(inner)
        T.TypeAnnotation.Paren(meta.typed(typedInner.meta.typ), typedInner)
    }
  }

  private def inferTypeAlias(alias: P.Declaration.TypeAlias): T.Declaration.TypeAlias = {
    alias match {
      case P.Declaration.TypeAlias(meta, modifiers, ident, None, Some(rhs)) =>
        val typedRhs = inferTypeAnnotation(rhs)
        val typedIdent = checkBindingVarIdent(typedRhs.meta.typ)(ident)
        val typedModifiers = modifiers.map(_.typed)
        T.Declaration.TypeAlias(
          meta.typed(typedIdent.meta.typ),
          typedModifiers,
          typedIdent,
          None,
          Some(typedRhs)
        )
    }
  }

  private def inferLetDeclaration(let: P.Declaration.Let): T.Declaration = {
    let match {
      case P.Declaration.Let(meta, modifiers, pattern, Some(rhs))
        if getPatternAnnotation(pattern).isDefined =>
        val typedPattern = inferPattern(pattern)
        val typedRhs = checkExpression(typedPattern.meta.typ)(rhs)
        val typedModifiers = modifiers.map(_.typed)
        T.Declaration.Let(
          meta.typed(typedPattern.meta.typ),
          typedModifiers,
          typedPattern,
          Some(typedRhs)
        )

      case P.Declaration.Let(meta, modifiers, pattern, Some(rhs)) =>
        val typedRhs = inferExpression(rhs)
        val typedPattern = checkPattern(typedRhs.meta.typ)(pattern)
        val typedModifiers = modifiers.map(_.typed)
        T.Declaration.Let(
          meta.typed(typedPattern.meta.typ),
          typedModifiers,
          typedPattern,
          Some(typedRhs)
        )
    }
  }

  private def getPatternAnnotation(p: P.Pattern): Option[P.TypeAnnotation] = {
    p match {
      case _: P.Pattern.Var => None
      case P.Pattern.Annotated(_, _, annotation) => Some(annotation)
      case P.Pattern.Paren(_, inner) => getPatternAnnotation(inner)
    }
  }

  private def inferPattern(pattern: P.Pattern): T.Pattern = {
    pattern match {
      case P.Pattern.Annotated(meta, innerPattern, typeAnnotation) =>
        val typedAnnotation = inferTypeAnnotation(typeAnnotation)
        val typ = evalTypeAnnotation(typedAnnotation)
        val typedInner =
            checkPattern(typ)(innerPattern)
        T.Pattern.Annotated(
          meta.typed(typ),
          typedInner,
          typedAnnotation
        )
      case P.Pattern.Paren(meta, inner) =>
        val typedInner = inferPattern(inner)
        T.Pattern.Paren(
          meta.typed(typedInner.meta.typ),
          typedInner
        )
      case P.Pattern.Var(meta, ident) =>
        val typedIdent = inferBindingVarIdent(ident)
        T.Pattern.Var(
          meta.typed(typedIdent.meta.typ),
          typedIdent
        )
    }
  }

  private def inferBindingVarIdent(ident: P.Ident): T.Ident = {
    val checkedIdent = checkBindingVarIdent(ctx.makeGenericType(ident.name))(ident)
    checkedIdent.copy(
      meta = checkedIdent.meta.withDiagnostic(
        Diagnostic(
          severity = Severity.Error,
          loc = ident.loc,
          variant = MissingTypeAnnotation
        )
      )
    )
  }

  private def checkExpression(expected: Type)(expr: P.Expression): T.Expression = {
    expr match {
      case f: P.Expression.Func =>
        checkFunc(expected)(f)
      case _ =>
        val typedExpr = inferExpression(expr)
        val errors = ListBuffer.empty[Diagnostic]
        assertAssignability(errors, expr.loc)(typedExpr.meta.typ, expected)
        typedExpr.withMeta(
          meta = typedExpr.meta.withDiagnostics(errors)
        )
    }
  }

  private def checkFunc(expected: Type)(f: P.Expression.Func): T.Expression = {
    applyEnv(expected) match {
      case Func(label, from, to) =>
        val typedGenericParams = inferGenericParams(f.genericParams)
        val expectedParams = getExpectedParams(expected)
        val expectedReturnTyp = getExpectedReturnType(expected)
        val errors = ListBuffer.empty[Diagnostic]
        var i = 0
        val typedParams = ListBuffer.empty[T.Expression.Param]
        for ((expectedLabel, expectedTyp) <- expectedParams) {
          if (i >= f.params.length) {
            // missing param
            errors.append(
              Diagnostic(
                severity = Severity.Error,
                loc = f.fnToken.loc,
                variant = MissingParam(
                  expectedLabel.map(_.text),
                  expectedTyp
                )
              )
            )
          } else {
            val param = f.params(i)
            val typedPattern = checkPattern(expectedTyp)(param.pattern)
            val labelError = (expectedLabel, getPatternLabelIdent(typedPattern)) match {
              case (Some(expectedL), Some(foundLabelIdent)) =>
                val foundLabel = foundLabelIdent.name
                if (expectedL.text != foundLabel.text) {
                  Some(Diagnostic(
                    severity = Severity.Error,
                    loc = foundLabelIdent.loc,
                    variant = FuncParamLabelMismatch(expectedL.text)
                  ))
                } else {
                  None
                }
              case (Some(expectedL), None) =>
                Some(Diagnostic(
                  severity = Severity.Error,
                  loc = typedPattern.loc,
                  variant = MissingParamLabel(expectedL.toString)
                ))
              case (None, _) =>
                None
            }
            typedParams.append(
              T.Expression.Param(
                typedPattern.withMeta(
                  meta = typedPattern.meta.withDiagnostics(labelError)
                )
              )
            )
          }
          i += 1
        }
        while (i < f.params.length) {
          val param = f.params(i)
          val typedPattern = inferPattern(param.pattern)
          typedParams.append(
            T.Expression.Param(
              typedPattern.withMeta(
                typedPattern.meta.withDiagnostic(
                  Diagnostic(
                    severity = Severity.Error,
                    loc = typedPattern.loc,
                    variant = ExtraParam
                  )
                )
              )
            )
          )
          i += 1
        }
        val typedRetTypAnnotation = f.returnType.map(checkTypeAnnotation(Star))
        typedRetTypAnnotation match {
          case Some(retTypAnnot) =>
            assertAssignability(
              errors, retTypAnnot.loc
            )(expectedReturnTyp, evalTypeAnnotation(retTypAnnot))
          case None => ()
        }
        val bodyExpectedTyp = typedRetTypAnnotation
          .map(evalTypeAnnotation)
          .getOrElse(expectedReturnTyp)
        val typedBody = checkExpression(bodyExpectedTyp)(f.body)
        T.Expression.Func(
          f.meta.typed(expected).withDiagnostics(errors),
          f.fnToken,
          f.scope,
          typedGenericParams,
          typedParams.toArray,
          typedRetTypAnnotation,
          typedBody
        )
    }
  }

  private def getExpectedParams(typ: Type): List[(Option[Symbol], Type)] = {
    typ match {
      case Func(label, from, to) =>
        (label, from)::getExpectedParams(to)
      case _ => List()
    }
  }

  @tailrec
  private def getExpectedReturnType(typ: Type): Type = {
    typ match {
      case Func(label, from, to) =>
        getExpectedReturnType(to)
      case t => t
    }
  }

  private def checkPattern(typ: Type)(pattern: P.Pattern): T.Pattern = {
    pattern match {
      case P.Pattern.Var(meta, ident) =>
        val typedIdent = checkBindingVarIdent(typ)(ident)
        T.Pattern.Var(meta.typed(typedIdent.meta.typ), typedIdent)
      case P.Pattern.Paren(meta, inner) =>
        val typedInner = checkPattern(typ)(inner)
        T.Pattern.Paren(
          meta.typed(typedInner.meta.typ),
          typedInner
        )
      case P.Pattern.Annotated(meta, inner, annotation) =>
        // annotation should be a type
        val typedAnnotation = checkTypeAnnotation(Star)(annotation)
        val annotType = evalTypeAnnotation(typedAnnotation)
        val errors = ListBuffer.empty[Diagnostic]
        assertAssignability(errors, annotation.loc)(annotType, typ)
        val typedInner = checkPattern(annotType)(inner)
        T.Pattern.Annotated(
          meta.typed(typ).withDiagnostics(errors),
          typedInner,
          typedAnnotation
        )
    }
  }

  private def evalTypeAnnotation(annotation: T.TypeAnnotation): Type = {
    annotation match {
      case T.TypeAnnotation.Var(_, ident) =>
        annotation.scope.resolveTypeVar(ident.name) match {
          case Some(t) => t
          case None =>
            Var(ident.name)
        }
      case T.TypeAnnotation.Func(_, _, label, from, returnType) =>
        Func(
          label.map(_.name),
          evalTypeAnnotation(from),
          evalTypeAnnotation(returnType)
        )
      case T.TypeAnnotation.Paren(_, inner) =>
        evalTypeAnnotation(inner)
    }
  }

  private def checkTypeAnnotation(t: Type)(annotation: P.TypeAnnotation) = {
    val typedAnnotation = inferTypeAnnotation(annotation)
    val errors = ListBuffer.empty[Diagnostic]
    assertAssignability(errors, annotation.loc)(typedAnnotation.meta.typ, t)
    typedAnnotation.withMeta(
      typedAnnotation.meta.withDiagnostics(errors)
    )
  }

  private def assertAssignability(errors: ListBuffer[diagnostics.Diagnostic], loc: Loc)
    (found: Type, expected: Type): Unit = {
    (applyEnv(found), expected) match {
      case
        (Primitive.Int, Primitive.Int)
        | (Primitive.Bool, Primitive.Bool)
        | (Primitive.Unit, Primitive.Unit)
        | (Star, Star)
      => ()
      case (_, ExistentialInstance(i, _)) =>
        _generics.update(i, found)
      case (Var(a), Var(b)) if a == b => ()
      case (Func(label1, from1, to1), Func(label2, from2, to2))
        if (
          label2.isEmpty ||
          label1.map(_.text) == label2.map(_.text)
        )=>
        assertAssignability(errors, loc)(to1, to2)
        assertAssignability(errors, loc)(from2, from1)
        ()
      case _ =>
        errors.append(
          Diagnostic(
            severity = Severity.Error,
            loc = loc,
            variant = NotAssignableTo(found, expected)
          )
        )
    }
  }

  private def inferExpression(expr: P.Expression): T.Expression = {
    import P.{Expression => E}
    expr match {
      case E.Literal(meta, E.Literal.LInt(x)) =>
        T.Expression.Literal(meta.typed(Primitive.Int), T.Expression.Literal.LInt(x))
      case E.Prop(meta, lhs, prop) =>
        val inferredLhs = inferExpression(lhs)
        applyEnv(inferredLhs.meta.typ) match {
          case m: Module =>
            m.symbolOfString.get(prop.name) match {
              case Some(symbol) =>
                m.values.get(symbol) match {
                  case Some(t) =>
                    val typedProp = T.Ident(prop.meta.typed(t), symbol)
                    T.Expression.Prop(
                      meta.typed(t),
                      inferredLhs,
                      typedProp
                    )
                  case None =>
                    // compiler bug
                    ???
                }
              case None =>
                // no such symbol
                ???
            }
          case _ =>
            // not a module
            ???
        }
      case E.Var(meta, ident) =>
        val typedIdent = inferIdent(ident, isType = false)

        T.Expression.Var(
          meta.typed(typedIdent.meta.typ),
          typedIdent
        )
      case f: E.Func => inferFunc(f)
    }
  }

  private def inferFunc(func: P.Expression.Func): T.Expression = {
    val checkedGenericParams = inferGenericParams(func.genericParams)
    val checkedParams = func.params.map(inferFuncParam)
    val checkedRetTyp = func.returnType.map(inferTypeAnnotation)
    val checkedBody = checkedRetTyp match {
      case Some(typeAnnotation) =>
        val t = evalTypeAnnotation(typeAnnotation)
        checkExpression(t)(func.body)
      case None =>
        inferExpression(func.body)
    }
    var typ = applyEnv(checkedBody.meta.typ)
    for (param <- checkedParams.reverse) {
      val label = getPatternLabel(param.pattern)
      typ = Func(label, param.pattern.meta.typ, typ)
    }
    typ match {
      case _: Func => ()
      case _ =>
        typ = Func(None, Primitive.Unit, typ)
    }
    T.Expression.Func(
      meta = func.meta.typed(typ),
      fnToken =  func.fnToken,
      func.scope,
      genericParams = checkedGenericParams,
      params = checkedParams,
      checkedRetTyp,
      checkedBody
    )
  }

  private def getPatternLabelIdent(pattern: T.Pattern): Option[T.Ident] = {
    pattern match {
      case T.Pattern.Var(_, ident) => Some(ident)
      case T.Pattern.Paren(_, inner) => getPatternLabelIdent(inner)
      case T.Pattern.Annotated(_, innerPattern, _) => getPatternLabelIdent(innerPattern)
      case T.Pattern.Error(_) => None
    }
  }

  private def getPatternLabel(pattern: T.Pattern): Option[Symbol] = {
    getPatternLabelIdent(pattern).map(_.name)
  }

  private def inferFuncParam(param: P.Expression.Param): T.Expression.Param = {
    val checkedPattern = inferPattern(param.pattern)
    T.Expression.Param(checkedPattern)
  }

  private def inferIdent(ident: P.Ident, isType: Boolean): T.Ident = {
    ident.scope.resolve(ident.name) match {
      case Some((typ, symbol)) =>
        val error = ctx.getDeclOf(symbol) match {
          case Some(d) if !isType && d.loc.start >= ident.loc.end =>
            Some(
              Diagnostic(
                loc = ident.loc,
                severity = Severity.Error,
                variant = UseBeforeAssignment(ident.name)
              )
            )
          case _ => None
        }
        T.Ident(ident.meta.typed(typ).withDiagnostics(error), symbol)
      case None =>
        T.Ident(
          ident.meta.typed(Uninferred).withDiagnostic(
            Diagnostic(
              severity = Severity.Error,
              loc = ident.loc,
              variant = UnBoundVar(ident.name)
            )
          ),
          ctx.makeSymbol(ident.name)
        )
    }
  }

  private def getModuleType(scope: Scope): Type = {
    Module(
      scope.typeMap.map({
        case (_, (t, sym)) =>
            sym -> t
      }).toMap
    )
  }

  private def assignHoles(node: P.Node): Unit = {
    def getSymbol(ident: P.Ident): Symbol = {
//      ctx.makeSymbol(ident.name)
      ident.scope.getSymbol(ident.name).get
    }
    node match {
      case p: P.Pattern.Var =>
        val symbol = getSymbol(p.ident)
        val typ = ctx.makeGenericType(p.ident.name)
        p.scope.setType(p.ident.name, symbol, typ)
      case m: P.Declaration.Module =>
        val symbol = getSymbol(m.ident)
        val typ = ctx.makeGenericType(m.ident.name)
        m.scope.setType(m.ident.name, symbol, typ)
      case m: P.Declaration.TypeAlias =>
        val symbol = getSymbol(m.ident)
        val typ = ctx.makeGenericType(m.ident.name)
        m.scope.setType(m.ident.name, symbol, typ)
      case _ => ()
    }
    node.children.foreach(assignHoles)

    node match {
      case m: P.Declaration.Module =>
        val modTyp = Module(
          m.moduleScope.typeMap.map({
            case (_, (t, sym)) => sym -> t
          }).toMap
        )
        val symbol = m.scope.typeMap.get(m.ident.name) match {
          case Some((ExistentialInstance(i, _), sym)) =>
            _generics.update(i, modTyp)
            sym
        }
        m.scope.setType(m.ident.name, symbol, modTyp)
      case _ => ()
    }
  }



  private def getErrors[N](errors: ListBuffer[Diagnostic], node: T.Node): Unit = {
    applyEnv(node.meta.typ, Some(errors))
    node.children.foreach(child => getErrors(errors, child))
  }


  def applyEnv(t: Type, errors: Option[ListBuffer[Diagnostic]] = None): Type = {
    t match {
      case ExistentialInstance(i, _) =>
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
        getTypeVar(symbol).map(t => applyEnv(t, errors)).getOrElse(v)
      case Module(values) =>
        Module(
          values.mapValues(ty => {
            applyEnv(ty, errors)
          })
        )
      case _ => t
    }
  }

  private def occursIn(symbol: Symbol, typ: Type): Boolean = {
    typ match {
      case Var(s) if s == symbol => true
      case Var(_) => false
      case Constructor(_) | Uninferred | ErrorType => false
      case Func(label, from, to) =>
        occursIn(symbol, from) ||
          occursIn(symbol, to)
      case TApplication(f, arg) =>
        occursIn(symbol, f) || occursIn(symbol, arg)
      case Module(values) =>
        values.values.exists(t => occursIn(symbol, t))
      case Forall(_, t) => occursIn(symbol, t)
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
              case Module(values) =>
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

