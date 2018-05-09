package lmc

import scala.collection.mutable
import lmc.syntax._
import lmc.common.{Loc, Symbol}
import lmc.diagnostics._
import lmc.types._
import lmc.utils.Debug

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

final class TypeChecker(
  private val ctx: Context.TC
) {
  import lmc.syntax.{Named => N, Typed => T}

  private val _checkedTypeAliasDecls =
    mutable.HashMap.empty[Symbol, T.Declaration]

  def inferSourceFile(parsed: Parsed.SourceFile): T.SourceFile = {
    val named = new Renamer(ctx).renameSourceFile(parsed)
    val inferredDeclarations = named.declarations.map(inferDeclaration)
    T.SourceFile(
      meta = parsed.meta.typed,
      scope = named.scope.typed,
      declarations = inferredDeclarations
    )
  }

  private def inferDeclaration(decl: N.Declaration): T.Declaration = {
    val result = decl.variant match {
      case N.Declaration.Let(pattern, rhs) =>
        val (checkedPattern, checkedRhs) = assignExprToPattern(pattern, rhs)
        T.Declaration(
          decl.meta.typed,
          T.Declaration.Let(
            checkedPattern, checkedRhs
          )
        )
      case N.Declaration.ExternLet(ident, annotation) =>
        Debug.log(s"inferDecl(extern $ident: $annotation)")
        val inferredAnnotation = inferAnnotation(annotation)
        val typ = annotationToType(inferredAnnotation)
        val inferredIdent = checkIdent(ident, typ)
        T.Declaration(
          decl.meta.typed,
          T.Declaration.ExternLet(inferredIdent, inferredAnnotation)
        )
      case N.Declaration.Existential(ident, kindAnnotation) =>
        inferTypeDeclHelper(ident, kindAnnotation, decl.meta, None)(
          (inferredIdent, inferredKindAnnotation, _, errors) => {
            T.Declaration(
              decl.meta.typed.withDiagnostics(errors),
              T.Declaration.Existential(inferredIdent, inferredKindAnnotation)
            )
          }
        )
      case N.Declaration.TypeAlias(ident, kindAnnotation, rhs) =>
        inferTypeDeclHelper(ident, kindAnnotation, decl.meta, Some(rhs))(
          (inferredIdent, inferredKindAnnotation, inferredTyp, errors) => {
            T.Declaration(
              decl.meta.typed.withDiagnostics(errors),
              T.Declaration.TypeAlias(inferredIdent, inferredKindAnnotation, inferredTyp.get)
            )
          }
        )
      case N.Declaration.Error() =>
        T.Declaration(
          decl.meta.typed,
          T.Declaration.Error()
        )
    }
    result
  }

  private def inferTypeDeclHelper(
    ident: Named.Ident,
    kindAnnotation: Option[Named.KindAnnotation],
    meta: N.Meta,
    typAnnot: Option[N.TypeAnnotation]
  )(
    f: (T.Ident, Option[T.KindAnnotation], Option[T.TypeAnnotation], Iterable[Diagnostic]) => T.Declaration
  ): T.Declaration = {
      _checkedTypeAliasDecls.get(ident.name) match {
        case Some(d) if d.meta.id == meta.id =>
          d
        case _ =>
          val inferredIdent = inferIdent(ident)
          val inferredKindAnnotation = kindAnnotation.map(inferKindAnnotation)
          val kind = inferredKindAnnotation.map(kindAnnotationToKind)
          val inferredAnnotation = typAnnot.map(checkTypeAnnotation(kind.getOrElse(Kind.Star)))
          val typ = inferredAnnotation.map(annotationToType)
          val errors = ListBuffer.empty[Diagnostic]
          setTypeVar(ident.loc, errors)(inferredIdent.name, inferredKindAnnotation, typ)
          val result = f(inferredIdent, inferredKindAnnotation, inferredAnnotation, errors)
          _checkedTypeAliasDecls.put(inferredIdent.name, result)
          result
      }
  }

  private def checkTypeAnnotation(expectedKind: Kind)(annotation: N.TypeAnnotation): T.TypeAnnotation = {
    val errors = ListBuffer.empty[Diagnostic]
    val inferred = inferAnnotation(annotation)
    Debug.log(s"checking kind eqality ${inferred.kind}, $expectedKind")
    if (inferred.kind != expectedKind) {
      addKindMismatchError(errors, annotation.loc)(expectedKind,  inferred.kind)
      inferred.copy(
        meta = inferred.meta.withDiagnostics(errors.toVector)
      )
    } else {
      inferred
    }
  }

  private def addKindMismatchError(errors: ListBuffer[Diagnostic], loc: Loc)
    (expectedKind: Kind, foundKind: Kind): Unit = {
    errors.append(
      Diagnostic(
        loc = loc,
        severity = Severity.Error,
        variant = KindMismatch(expectedKind, foundKind)
      )
    )
  }


  private def setTypeVar(
    loc: Loc,
    errors: ListBuffer[diagnostics.Diagnostic]
  )(symbol: Symbol, kindAnnotation: Option[T.KindAnnotation], typOpt: Option[Type] = None): Unit = {

    val kind = kindAnnotation.map(kindAnnotationToKind).getOrElse(Kind.Star)
    Debug.log(s"setTypeVar($symbol, $kind, $typOpt)")
    ctx.setKindOfSymbol(symbol, kind)
    typOpt match  {
      case None => ()
      case Some(typ) =>
        if (occursIn(symbol, typ)) {
          errors.append(
            Diagnostic(
              loc = loc,
              severity = Severity.Error,
              variant = CyclicType
            )
          )
        } else {
          ctx.setTypeVar(symbol, typ)
        }
    }
  }

  private def kindAnnotationToKind(annot: T.KindAnnotation): Kind = {
    annot.variant match {
      case T.KindAnnotation.Star => Kind.Star
      case T.KindAnnotation.KFun(from, to) =>
        from.foldRight(kindAnnotationToKind(to))(
          (current, prev) => Kind.KFun(kindAnnotationToKind(current), prev)
        )
      case T.KindAnnotation.Error => Kind.Star
    }
  }

  private def occursIn(symbol: Symbol, t: Type): Boolean = {
    t match {
      case Var(sym) if symbol.id == sym.id => true
      case Var(_)  => false
      case (
        Primitive.Unit
        | Primitive.Bool
        | Primitive.Int
        | ErrorType
      ) => false
      case Func(from, to) =>
        occursIn(symbol, to) || from.exists(t => occursIn(symbol, t._2))
      case Forall(_, typ) =>
        occursIn(symbol, typ)
      case TApplication(f, arg) =>
        occursIn(symbol, f) || occursIn(symbol, arg)
      case ExistentialInstance(_, _) => false
    }
  }

  private def assignExprToPattern(pattern: N.Pattern, expr: N.Expr): (T.Pattern, T.Expr) = {
    pattern.variant match {
      case N.Pattern.Annotated(pat, annotation) =>
        val inferredAnnotation = inferAnnotation(annotation)
        val expectedType = annotationToType(inferredAnnotation)
        val checkedPattern = checkPattern(pat, expectedType)
        val checkedExpr = checkExpr(expr, expectedType)
        (T.Pattern(
          pattern.meta.typed,
          typ = checkedPattern.typ,
          T.Pattern.Annotated(
            checkedPattern,
            inferredAnnotation
          )
        ), checkedExpr)
      case _ =>
        val inferredExpr = inferExpr(expr)
        val checkedPattern = checkPattern(pattern, inferredExpr.typ)
        (checkedPattern, inferredExpr)
    }
  }

  private def inferExpr(expr: N.Expr): T.Expr = {
    expr.variant match {
      case N.Expr.Var(ident) =>
        val inferredIdent = inferIdent(ident)
        val typ = ctx.getTypeOfSymbol(inferredIdent.name) match {
          case Some(t) =>
            t
          case None =>
            ErrorType
        }
        T.Expr(
          meta = expr.meta.typed,
          typ = typ,
          variant = T.Expr.Var(inferredIdent)
        )

      case N.Expr.Literal(N.Expr.LInt(value)) =>
        T.Expr(
          meta = expr.meta.typed,
          typ = Primitive.Int,
          variant = T.Expr.Literal(T.Expr.LInt(value))
        )
      case func@N.Expr.Func(_, _, _, _, _, _) =>
        inferFunc(expr, func)
      case call@N.Expr.Call(_, _, _) =>
        inferCall(expr, call)
      case N.Expr.Error() =>
        T.Expr(
          meta = expr.meta.typed,
          typ = ErrorType,
          variant = T.Expr.Error()
        )
    }
  }

  private def instantiateForall(typ: Type): Type = {
    typ match {
      case Forall(param, innerTyp) =>
        val subst = mutable.Map.empty[Symbol, Type]
        subst.put(param, ctx.makeGenericType(param.text))
        instantiateForall(applySubst(innerTyp, subst))
      case _ => typ
    }
  }

  private def inferCall(expr: N.Expr, call: N.Expr.Call): T.Expr = {
    val inferredFunc = inferExpr(call.func)
    val typ = instantiateForall(inferredFunc.typ)
    val inferredExpr = typ match {
      case Func(from, to) =>
        val expectedLabeledArgs = from.filter({ _._1.isDefined }).map(arg => {
          (arg._1.get.text, arg._2)
        }).toMap
        val labelChecked = mutable.Set.empty[String]
        var i = 0
        val checkedArgs = ListBuffer.empty[T.Expr.Arg]
        val missingArguments = ListBuffer.empty[(Option[String], Type)]
        var labelEncountered = false
        val labeledArgsToCheck = ListBuffer.empty[((token.Token, String), N.Expr)]
        for (expectedArg <- from) {
          if (i >= call.args.length) {
            missingArguments.append(expectedArg._1.map(_.text) -> expectedArg._2)
            expectedArg._1 match {
              case Some(s) => labelChecked += s.text
              case None => ()
            }
          } else {
            val arg = call.args(i)
            arg.label match {
              case Some(label) =>
                labelEncountered = true
                labeledArgsToCheck.append(label -> arg.value)
              case None =>
                val diagnostic = if (labelEncountered) {
                  Some(Diagnostic(
                    loc = arg.value.loc,
                    severity = Severity.Error,
                    variant = PositionalArgAfterLabelled
                  ))
                } else {
                  None
                }
                expectedArg._1 match {
                  case None => ()
                  case Some(label) =>
                    labelChecked += label.text
                }
                var checkedExpr = checkExpr(arg.value, expectedArg._2)
                diagnostic match {
                  case Some(d) =>
                    checkedExpr = checkedExpr.copy(
                      meta = checkedExpr.meta.withDiagnostic(d)
                    )
                  case None => ()
                }
                val checkedArg = T.Expr.Arg(None, checkedExpr)
                checkedArgs.append(checkedArg)
            }
          }
          i += 1
        }
        while (i < call.args.length) {
          val arg = call.args(i)
          arg.label match {
            case Some(label) =>
              labelEncountered = true
              labeledArgsToCheck.append(label -> arg.value)
            case None =>
              var inferredExpr = inferExpr(arg.value)
              inferredExpr = inferredExpr.copy(
                meta = inferredExpr.meta.withDiagnostic(
                  Diagnostic(
                    loc = arg.value.loc,
                    severity = Severity.Error,
                    variant = ExtraArg
                  )
                )
              )
              val checkedArg = T.Expr.Arg(None, inferredExpr)
              checkedArgs.append(checkedArg)
          }
          i += 1
        }
        for (((tok, name), expr) <- labeledArgsToCheck) {
          expectedLabeledArgs.get(name) match {
            case Some(expectedTyp) =>
              val diagnostic = if (labelChecked.contains(name)) {
                Some(Diagnostic(
                  loc = tok.loc,
                  severity = Severity.Error,
                  variant = DuplicateLabelArg(name)
                ))
              } else {
                None
              }
              var checkedExpr = checkExpr(expr, expectedTyp)
              diagnostic match {
                case Some(d) =>
                  checkedExpr = checkedExpr.copy(
                    meta = checkedExpr.meta.withDiagnostic(d)
                  )
                case None => ()
              }
              val checkedArg = T.Expr.Arg(Some(tok -> name), checkedExpr)
              checkedArgs.append(checkedArg)
              labelChecked += name
            case None =>
              var inferredExpr = inferExpr(expr)
              inferredExpr = inferredExpr.copy(
                meta = inferredExpr.meta.withDiagnostic(
                  Diagnostic(
                    loc = tok.loc,
                    severity = Severity.Error,
                    variant = NoSuchParamLabel(name)
                  )
                )
              )
              val inferredArg = T.Expr.Arg(
                label = Some(tok -> name),
                value = inferredExpr
              )
              checkedArgs.append(inferredArg)
          }
        }
        for (expected <- from) {
          expected._1 match {
            case Some(name) =>
              if (!labelChecked.contains(name.text)) {
                missingArguments.append(Some(name.text) -> expected._2)
              }
            case None => ()
          }
        }
        val meta = if (missingArguments.isEmpty)
          expr.meta.typed
        else
          expr.meta.typed.withDiagnostic(
            Diagnostic(
              loc = call.argsLoc,
              variant = MissingArguments(missingArguments.toList),
              severity = Severity.Error
            )
          )
        T.Expr(
          meta = meta,
          typ = to,
          variant = T.Expr.Call(
            call.argsLoc,
            inferredFunc,
            checkedArgs.toVector
          )
        )
      case _ =>
        val inferredArgs = inferArgs(call.args)
        T.Expr(
          meta = expr.meta.withDiagnostic(
            Diagnostic(
              loc = inferredFunc.loc,
              severity = Severity.Error,
              variant = NotAFunction(inferredFunc.typ)
            )
          ).typed,
          typ = ErrorType,
          variant = T.Expr.Call(
            call.argsLoc,
            inferredFunc.copy(),
            inferredArgs
          )
        )
    }
    inferredExpr.copy(
      typ = applyEnv(inferredExpr.typ)
    )
  }

  private def applyEnv(typ: Type): Type = {
    typ match {
      case (
        Primitive.Unit
        | Primitive.Bool
        | Primitive.Int
        | ErrorType
        ) => typ
      case ExistentialInstance(id, _) =>
        ctx.getGeneric(id).getOrElse(typ)
      case Var(name) =>
        ctx.getTypeOfSymbol(name).getOrElse(typ)
      case Func(from, to) =>
        Func(
          from.map(p => {
            (p._1, applyEnv(p._2))
          }),
          applyEnv(to)
        )
      case TApplication(f, arg) =>
        TApplication(applyEnv(f), applyEnv(arg))
      case Forall(params, inner) =>
        Forall(
          params,
          applyEnv(inner)
        )
    }
  }

  private def inferArgs(args: Vector[Named.Expr.Arg]): Vector[T.Expr.Arg] = {
    args.map(arg => {
      T.Expr.Arg(label = arg.label, value = inferExpr(arg.value))
    })
  }

  private def inferFunc(expr: N.Expr, func: N.Expr.Func): T.Expr = {
    var inferringPositional = true
    val inferredGenericParams = inferGenericParams(func.genericParams)
    val inferredParams = func.params.map(param => {
      var inferredPattern = inferPattern(param.pattern)
      getPatternLabel(inferredPattern) match {
        case Some(_) =>
          inferringPositional = false
        case None =>
          if (inferringPositional) {
            inferredPattern = inferredPattern.copy(
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
      T.Expr.Param(inferredPattern)
    })
    val (checkedBody, checkedReturnTypeAnnotation) = func.returnTypeAnnotation match {
      case Some(t) =>
        val checkedRetTypAnnotation = checkTypeAnnotation(Kind.Star)(t)
        val expectedRetTyp = annotationToType(checkedRetTypAnnotation)
        (checkExpr(func.body, expectedRetTyp), Some(checkedRetTypAnnotation))
      case None =>
        (inferExpr(func.body), None)
    }
    val from = inferredParams.map(param => {
      (getPatternLabel(param.pattern), param.pattern.typ)
    }).toVector
    val innerTyp = Func(from, checkedBody.typ)
    val typ = if (func.genericParams.isEmpty) {
      innerTyp
    } else {
      makeForall(inferredGenericParams.map(_.ident.name), innerTyp)
    }
    T.Expr(
      meta = expr.meta.typed,
      typ = typ,
      variant = T.Expr.Func(
        func.fnTok,
        func.scope,
        Vector.empty,
        inferredParams,
        checkedReturnTypeAnnotation,
        checkedBody
      )
    )
  }


  private def inferPattern(p: N.Pattern): T.Pattern = {
    val (variant: T.Pattern.Variant, typ: Type, errors): (T.Pattern.Variant, Type, Iterable[Diagnostic]) =
      p.variant match {
        case N.Pattern.Var(ident) =>
          val t = ctx.makeGenericType(ident.name.text)
          val checkedIdent = checkIdent(ident, t)
          val diagnostic = Diagnostic(
            loc = p.loc,
            severity = Severity.Error,
            variant = MissingTypeAnnotation
          )
          (
            T.Pattern.Var(checkedIdent),
            t,
            List(diagnostic)
          )
        case N.Pattern.Annotated(inner, annotation) =>
          val inferredAnnotation = checkTypeAnnotation(Kind.Star)(annotation)
          val annotationTyp = annotationToType(inferredAnnotation)
          val checkedPattern = checkPattern(inner, annotationTyp)
          (
            T.Pattern.Annotated(checkedPattern, inferredAnnotation),
            annotationTyp,
            List.empty
          )
        case N.Pattern.Error =>
          (T.Pattern.Error, ErrorType, List.empty)
      }
    T.Pattern(
      meta = p.meta.withDiagnostics(errors).typed,
      typ = typ,
      variant = variant
    )
  }

  private def getPatternLabel(pattern: T.Pattern): Option[Symbol] = {
    pattern.variant match {
      case T.Pattern.Var(ident) => Some(ident.name)
      case T.Pattern.Annotated(p, _) =>
        getPatternLabel(p)
      case _ => None
    }
  }
  private def getPatternLabelStr(pattern: N.Pattern): Option[Symbol] = {
    pattern.variant match {
      case N.Pattern.Var(ident) => Some(ident.name)
      case N.Pattern.Annotated(p, _) =>
        getPatternLabelStr(p)
      case _ => None
    }
  }

  private def checkExpr(expr: N.Expr, typ: Type): T.Expr = {
    (expr.variant, typ) match {
      case (
        N.Expr.Error()
        | N.Expr.Call(_, _, _)
        | N.Expr.Literal(_)
        | N.Expr.Var(_),
        _
      )=>
        val inferredExpr = inferExpr(expr)
        val errors = assertSubType(expr.meta)(inferredExpr.typ, typ)
        inferredExpr.copy(
          meta = inferredExpr.meta.withDiagnostics(errors)
        )
      case (func@N.Expr.Func(_, _, _, _, _, _), _) =>
        checkFunc(expr, func, typ)
      case (_, Forall(_, innerTyp)) =>
        checkExpr(expr, innerTyp)
    }
  }

  private def checkFunc(expr: N.Expr, func: Named.Expr.Func, typ: Type): T.Expr = {
    typ match {
      case Forall(expectedParam, innerTyp) =>
        val errors = ListBuffer.empty[Diagnostic]
        if (func.genericParams.length < 1) {
          ()
        } else {
          val funcGenericParam = func.genericParams.head
          val inferredKindAnnot = funcGenericParam.kindAnnotation.map(inferKindAnnotation)
          setTypeVar(
            funcGenericParam.loc, errors
          )(funcGenericParam.ident.name, inferredKindAnnot, Some(Var(expectedParam)))
        }
        val f = checkFunc(expr, func.copy(
          genericParams =
            if (func.genericParams.nonEmpty)
              func.genericParams.tail
            else
              Vector.empty
        ), innerTyp)
        if (errors.isEmpty) {
          f
        } else {
          f.copy(
            meta = f.meta.withDiagnostics(errors)
          )
        }
      case Func(from, to) =>
        val funcParamsVec = func.params.toVector
        var i = 0
        val checkedParams = ListBuffer.empty[T.Expr.Param]
        for ((expectedParamLabel, expectedParamTyp) <- from) {
          if (i < funcParamsVec.length) {
            val funcParam = funcParamsVec(i)
            var checkedPattern = checkPattern(funcParam.pattern, expectedParamTyp)
            expectedParamLabel match {
              case Some(expectedLabelSym) =>
                if (!getPatternLabel(checkedPattern).map(_.text).contains(expectedLabelSym.text)) {
                  checkedPattern = checkedPattern.copy(
                    meta = checkedPattern.meta.withDiagnostic(
                      Diagnostic(
                        loc = checkedPattern.loc,
                        severity = Severity.Error,
                        variant = FuncParamLabelMismatch(expectedLabelSym.text)
                      )
                    )
                  )
                }
              case None =>
                ()
            }
            checkedParams.append(T.Expr.Param(checkedPattern))
          }
          i += 1
        }
        while (i < funcParamsVec.length) {
          val funcParam = funcParamsVec(i)
          var inferredPattern = inferPattern(funcParam.pattern)
          inferredPattern = inferredPattern.copy(
            meta = inferredPattern.meta.withDiagnostic(
              Diagnostic(
                loc = inferredPattern.loc,
                severity = Severity.Error,
                variant = ExtraParam
              )
            )
          )
          checkedParams.append(T.Expr.Param(inferredPattern))
          i += 1
        }
        val checkedRetTypAnnotation = func.returnTypeAnnotation.map({ checkAnnotation(_, to) })
        val checkedBody = checkedRetTypAnnotation match {
          case Some(annot) =>
            val t = annotationToType(annot)
            checkExpr(func.body, t)
          case None =>
            checkExpr(func.body, to)
        }
        val checkedGenericParams = inferGenericParams(func.genericParams).toVector
        T.Expr(
          meta = expr.meta.typed,
          typ = typ,
          variant = T.Expr.Func(
            func.fnTok,
            func.scope,
            checkedGenericParams,
            checkedParams,
            checkedRetTypAnnotation,
            checkedBody
          )
        )
      case _ =>
        val e = inferExpr(expr)
        e.copy(
          meta = e.meta.withDiagnostic(
            Diagnostic(
              loc = e.loc,
              severity = Severity.Error,
              variant = TypeMismatch(
                typ,
                e.typ
              )
            )
          )
        )
    }
  }

  private def inferGenericParams(params: Iterable[Named.GenericParam]): Iterable[T.GenericParam] = {
    params.map(param => {
      val kindAnnotation = param.kindAnnotation.map(inferKindAnnotation)
      val kind = kindAnnotation.map(kindAnnotationToKind).getOrElse(Kind.Star)
      ctx.setKindOfSymbol(param.ident.name, kind)
      Debug.log(s"Assigning kind ${param.ident}: $kind")
      T.GenericParam(
        meta = param.meta.typed,
        kind = kind,
        kindAnnotation = kindAnnotation,
        ident = inferIdent(param.ident)
      )
    })
  }

  /**
    * Checks if given type is a Var which is defined later in the
    * file. If it is, check the declaration and cache it.
    *
    * e.g.
    * let a: A = 23;
    * type A = Int
    *
    * While checking let a ..., the type variable A is
    * unresolved. So, we check if there's an unchecked declaration
    * with the name A. In this case, there is (type A = Int) so,
    * we check it which will add mapping A -> Int in context type vars.
    * Because the mapping is now added, we can check 23 with Int.
    * When we move to the type alias declaration, we already have the
    * type checked version of it in cache so we simply return that.
    * resolveForwardAlias(_, Var(A))
    * @param meta
    * @param typ
    * @return
    */
  private def resolveForwardAlias(meta: N.Meta, typ: Type): Type = {
    typ match {
      case Var(v) =>
        meta.scope.get match {
          case Some(scope) =>
            scope.declMap.get(v).flatMap(_.get) match {
              case Some(
                decl@N.Declaration(_,
                  (N.Declaration.TypeAlias(_, _, _)
                  | N.Declaration.Existential(_, _))
                )) =>
                inferDeclaration(decl)
                val resolved = resolveType(typ)
                resolved
              case _ => typ
            }
          case None => typ
        }
      case _ => typ
    }

  }
  private def assertSubType(meta: Named.Meta, exact: Boolean = false)(a: Type, b: Type): Iterable[Diagnostic] = {
    _assertSubType(meta, exact, ListBuffer.empty)(a, b)
  }

  private def _assertSubType(
    meta: N.Meta, exact: Boolean = false,
    errors: ListBuffer[Diagnostic]
  )(_a: Type, _b: Type): Iterable[Diagnostic] = {

    val a = resolveForwardAlias(meta, resolveType(_a))
    val b = resolveForwardAlias(meta, resolveType(_b))

    Debug.log(s"checking if $a <: $b (${_a} <: ${_b})")

    (a, b) match {
      case (Primitive.Int, Primitive.Int) |
           (Primitive.Bool, Primitive.Bool) |
           (Primitive.Unit, Primitive.Unit)
      =>
        ()
      case (Var(as), Var(bs)) if as.id == bs.id =>
        ()
      case (ExistentialInstance(a1, _), ExistentialInstance(b1, _)) if a1 == b1 =>
        ()
      case (_, existential@ExistentialInstance(_, _)) =>
        instantiateR(meta, errors)(a, existential)
      case (existential@ExistentialInstance(_, _), _) =>
        instantiateL(meta, errors)(existential, b)
      case (Func(aFrom, aTo), Func(bFrom, bTo)) =>
        // TODO: Check param types; each bFrom <: aFrom
        _assertSubType(meta, errors = errors)(aTo, bTo)
      case (Forall(param, innerTyp), _) =>
        val subst = mutable.Map.empty[Symbol, Type]
        subst.put(param, ctx.makeGenericType(param.text))
        val substituted = applySubst(innerTyp, subst)
        _assertSubType(meta, exact, errors)(substituted, b)
      case (TApplication(aF, aArg), TApplication(bF, bArg)) =>
        _assertSubType(meta, errors = errors)(aF, bF)
        // Type application is invariant; i.e.
        // List[Super] is neither a super type, nor a sub type
        // of List[Sub] if Sub <: Super
        _assertSubType(meta, exact = true, errors)(aArg, bArg)
      case (_, _) =>
        errors.append(
          Diagnostic(
            TypeMismatch(b, a),
            Severity.Error,
            meta.loc
          )
        )
    }
    errors
  }

  private def instantiateL(meta: N.Meta, errors: ListBuffer[Diagnostic])(existential: ExistentialInstance, t: Type): Iterable[Diagnostic] = {
    ctx.getGeneric(existential.id) match {
      case Some(a) =>
        _assertSubType(meta, errors = errors)(a, t)
      case None =>
        ctx.assignGeneric(existential.id, t)
        List.empty
    }
  }

  private def instantiateR(meta: N.Meta, errors: ListBuffer[Diagnostic])
    (a: Type, existential: ExistentialInstance): Iterable[Diagnostic] = {
    ctx.getGeneric(existential.id) match {
      case Some(b) =>
        _assertSubType(meta, errors = errors)(a, b)
      case None =>
        ctx.assignGeneric(existential.id, a)
        List.empty
    }
  }

  private def applySubst(t: Type, subst: collection.Map[Symbol, Type]): Type = {
    t match {
      case (
        Primitive.Bool
        | Primitive.Int
        | Primitive.Unit
        | ErrorType
        | ExistentialInstance(_, _)
      ) => t
      case Var(name) =>
        subst.get(name) match {
          case Some(typ) =>
            applySubst(typ, subst)
          case None => t
        }
      case Forall(params, inner) =>
        Forall(params, applySubst(inner, subst))
      case TApplication(f, arg) =>
        TApplication(applySubst(f, subst), applySubst(arg, subst))
      case Func(from, to) =>
        Func(
          from.map(param => (param._1, applySubst(param._2, subst))),
          applySubst(to, subst)
        )
    }
  }

  private def checkPattern(pattern: N.Pattern, typ: Type): T.Pattern = {
    pattern.variant match {
      case N.Pattern.Var(ident) =>
        val checkedIdent = checkIdent(ident, typ)
        T.Pattern(
          meta = pattern.meta.typed,
          typ = typ,
          variant = T.Pattern.Var(
            checkedIdent
          )
        )
      case N.Pattern.Annotated(inner, annotation) =>
        val checkedAnnotation = checkAnnotation(annotation, typ)
        val patTyp = annotationToType(checkedAnnotation)
        val checkedInnerPattern = checkPattern(inner, patTyp)
        T.Pattern(
          meta = pattern.meta.typed,
          typ = patTyp,
          variant = T.Pattern.Annotated(
            checkedInnerPattern,
            checkedAnnotation
          )
        )

      case N.Pattern.Error =>
        T.Pattern(
          meta = pattern.meta.typed,
          typ = typ,
          variant = T.Pattern.Error
        )
    }
  }

  private def checkIdent(ident: Named.Ident, typ: Type): T.Ident = {
    (ident.duplicateBinder, ctx.getTypeOfSymbol(ident.name)) match {
      case (false, Some(t)) =>
        throw new Error(
          s"Compiler bug: Tried to assign type to already assigned ident $ident".concat(
            s" (loc: ${ident.loc}, old type: $t; new type: $typ)"
          )
        )
      case _ => ()
    }
    val resolvedType = resolveForwardAlias(ident.meta, typ)
    ctx.setTypeOfSymbol(ident.name, resolvedType)
    T.Ident(
      ident.meta.typed,
      ident.name
    )
  }

  private def resolveType(typ: Type): Type = {
    typ match {
      case TApplication(Forall(param, t), arg) =>
        val subst = Map(param -> arg)
        resolveType(applySubst(t, subst))
      case Var(name) =>
        ctx.getTypeVar(name) match {
          case Some(t) =>
            resolveType(t)
          case None => typ
        }
      case _ => typ
    }
  }

  private def inferAnnotation(annotation: N.TypeAnnotation): T.TypeAnnotation = {
    val (variant: T.TypeAnnotation.Variant, kind) = annotation.variant match {
      case N.TypeAnnotation.Var(ident) =>
        val inferredIndent = inferIdent(ident)
        val k = ctx.getKindOfSymbol(inferredIndent.name).getOrElse(Kind.Star)
        (T.TypeAnnotation.Var(inferredIndent), k)
      case N.TypeAnnotation.Forall(scope, params, innerAnnotation) =>
        val inferredInner = inferAnnotation(innerAnnotation)
        val inferredParams = inferGenericParams(params.toVector)
        (
          T.TypeAnnotation.Forall(scope, inferredParams, inferredInner),
          makeKFun(inferredParams.map(_.kind), inferredInner.kind)
        )
      case N.TypeAnnotation.Func(params, retType) =>
        val inferredParams = params.map(param => {
          (param._1.map(inferIdent), inferAnnotation(param._2))
        })
        val checkedRetTypAnnotation = checkTypeAnnotation(Kind.Star)(retType)
        (
          T.TypeAnnotation.Func(
            inferredParams,
            checkedRetTypAnnotation
          ),
          Kind.Star
        )
      case N.TypeAnnotation.TApplication(f, args) =>
        val inferredF = inferAnnotation(f)
        val inferredArgs = args.map(inferAnnotation)
        val expectedArgKinds = getKindParamArgs(inferredF.kind)
        val checkedArgs =
          inferredArgs
            .take(expectedArgKinds.length)
            .zip(expectedArgKinds)
            .map(tuple => {
              val errors = ListBuffer.empty[Diagnostic]
              val (arg, expectedKind) = tuple
              if (arg.kind == expectedKind) {
                arg
              } else {
                addKindMismatchError(errors, arg.loc)(expectedKind, arg.kind)
                arg.copy(
                  meta = arg.meta.withDiagnostics(errors)
                )
              }
            }) ++
          inferredArgs
            .drop(expectedArgKinds.length)
            .map(arg => arg.copy(
                meta = arg.meta.withDiagnostic(
                  Diagnostic(
                    loc = arg.loc,
                    severity = Severity.Error,
                    variant = ExtraTypeArg
                  )
                )
              )
            )
        val k = kindOfApplication(inferredF.kind, checkedArgs.size)
        (
          T.TypeAnnotation.TApplication(
            inferredF,
            checkedArgs
          ),
          k
        )

      case N.TypeAnnotation.Error =>
        (T.TypeAnnotation.Error, Kind.Star)
    }
    T.TypeAnnotation(
      meta = annotation.meta.typed,
      kind = kind,
      variant
    )
  }

  @tailrec private def kindOfApplication(kind: Kind, numArgs: Int): Kind = {
    if (numArgs == 0) {
      kind
    } else {
      kind match {
        case Kind.Star => Kind.Star
        case Kind.KFun(_, k) =>
          kindOfApplication(k, numArgs - 1)
      }
    }
  }

  private def getKindParamArgs(kind: Kind): Vector[Kind] = {
    kind match {
      case Kind.Star => Vector.empty
      case Kind.KFun(param, to) =>
        param +: getKindParamArgs(to)
    }
  }

  private def makeKFun(paramKinds: Iterable[Kind], resultKind: Kind): Kind = {
    paramKinds.foldRight(resultKind)((current, prev) => Kind.KFun(current, prev))
  }

  private def makeForall(paramNames: Iterable[Symbol], resultType: Type): Type = {
    paramNames.foldRight(resultType)((current, prev) => Forall(current, prev))
  }


  private def inferKindAnnotation(annot: Named.KindAnnotation): T.KindAnnotation = {
    val variant = annot.variant match {
      case N.KindAnnotation.Star => T.KindAnnotation.Star
      case N.KindAnnotation.KFun(from, to) =>
        T.KindAnnotation.KFun(
          from.map(inferKindAnnotation),
          inferKindAnnotation(to)
        )
      case N.KindAnnotation.Error => T.KindAnnotation.Error
    }
    T.KindAnnotation(
      meta = annot.meta.typed,
      variant
    )
  }

  private def inferIdent(ident: N.Ident): T.Ident = {
    T.Ident(
      meta = ident.meta.typed,
      name = ident.name,
      duplicateBinder = ident.duplicateBinder
    )
  }

  private def checkAnnotation(annotation: N.TypeAnnotation, typ: Type): T.TypeAnnotation = {
    val inferredAnnotation = inferAnnotation(annotation)
    val annotTyp = annotationToType(inferredAnnotation)
    val errors = assertSubType(annotation.meta)(annotTyp, typ)
    inferredAnnotation.copy(
      meta = inferredAnnotation.meta.withDiagnostics(errors)
    )
  }

  private def annotationToType(annotation: T.TypeAnnotation): Type = {
    resolveType(annotation.variant match {
      case T.TypeAnnotation.Var(ident) =>
        Var(ident.name)
      case T.TypeAnnotation.Func(params, to) =>
        Func(
          params.map(param => {
            (param._1.map(_.name), annotationToType(param._2))
          }).toVector,
          annotationToType(to)
        )
      case T.TypeAnnotation.Forall(_, params, inner) =>
        val typParams = params.map(_.ident.name)
        makeForall(typParams, annotationToType(inner))
      case T.TypeAnnotation.TApplication(f, args) =>
        args.foldLeft(annotationToType(f))(
          (func, arg) => TApplication(func, annotationToType(arg))
        )
      case T.TypeAnnotation.Error =>
        ErrorType
    })
  }
}
