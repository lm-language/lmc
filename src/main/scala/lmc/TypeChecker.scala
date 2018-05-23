package lmc

import scala.collection.mutable
import lmc.syntax._
import lmc.common.{Loc, ScopeEntry, Symbol}
import lmc.diagnostics._
import lmc.types._
import lmc.utils.Debug

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference

final class TypeChecker(
  private val ctx: Context.TC
) {
  import lmc.syntax.{Named => N, Typed => T}

  private val Primitive = ctx.Primitive

  private val _checkedTypeAliasDecls =
    mutable.HashMap.empty[Symbol, WeakReference[T.Declaration]]

  private val _checkedDecls = mutable.HashMap.empty[Int, WeakReference[T.Declaration]]

  private val _checkedExprs =
    mutable.HashMap.empty[Int, WeakReference[T.Expr]]

  def inferSourceFile(parsed: Parsed.SourceFile): T.SourceFile = {
    val named = new Renamer(ctx.asInstanceOf[Context.Renamer]).renameSourceFile(parsed)
    val inferredDeclarations = named.declarations.map(inferDeclaration)
    T.SourceFile(
      meta = parsed.meta.typed,
      scope = named.scope.typed,
      declarations = inferredDeclarations
    )
  }

  private def inferDeclaration(decl: N.Declaration): T.Declaration = {
    _checkedDecls.get(decl.meta.id).flatMap(_.get) match {
      case Some(d) =>
        d
      case None =>
        val inferred = inferDeclarationHelper(decl)
        _checkedDecls.update(decl.meta.id, WeakReference(inferred))
        inferred
    }
  }

  private def inferDeclarationHelper(decl: N.Declaration): T.Declaration = {
    val modifiers = decl.modifiers.map(N.Declaration.Modifier.typed)
    val result = decl.variant match {
      case N.Declaration.Let(pattern, rhs) =>
        val (checkedPattern, checkedRhs) = rhs match {
          case Some(e) =>
            val (p, e1) = assignExprToPattern(pattern, e)
            (p, Some(e1))
          case None =>
            (inferPattern(pattern), None)
        }
        T.Declaration(
          decl.meta.typed,
          T.Declaration.Let(
            checkedPattern, checkedRhs
          ),
          modifiers
        )
      case N.Declaration.Include(expr) =>
        val inferredExpr = inferExpr(expr)
        val errors = resolveType(inferredExpr.typ) match {
          case Module(_, _) => List()
          case _ => List(
            Diagnostic(
              loc = expr.loc,
              severity = Severity.Error,
              variant = TriedToIncludeNonModule(inferredExpr.typ)
            )
          )
        }
        T.Declaration(
          decl.meta.typed.withDiagnostics(errors),
          T.Declaration.Include(
            inferredExpr
          ),
          modifiers
        )
      case N.Declaration.TypeAlias(ident, kindAnnotation, rhs) =>
        _checkedTypeAliasDecls.get(ident.name).flatMap(_.get) match {
          case Some(d) if d.meta.id == decl.meta.id =>
            d
          case _ =>
            val errors = ListBuffer.empty[Diagnostic]
            val inferredIdent = inferIdent(ident)
            val inferredKindAnnotation = kindAnnotation.map(inferKindAnnotation)
            val expectedKind = inferredKindAnnotation
              .map(kindAnnotationToKind)
              .getOrElse(Kind.Star)
            var inferredRHS = rhs.map(annot => checkTypeAnnotation(annot, expectedKind))
            if (decl.isAbstract || decl.isExtern) {
              inferredRHS = inferredRHS match {
                case Some(r) =>
                  Some(r.copy(
                    meta = r.meta.withDiagnostic(
                      Diagnostic(
                        loc = r.meta.loc,
                        severity = Severity.Error,
                        variant = UnexpectedBodyInAbstract
                      )
                    )
                  ))
                case None => None
              }
            } else {
              if (inferredRHS.isEmpty) {
                  errors.append(
                    Diagnostic(
                      loc = decl.loc,
                      severity = Severity.Error,
                      variant = MissingBodyInNonAbstract
                    )
                  )
              }
            }
            val typ = inferredRHS.map(annotationToType)
            setTypeVar(ident.loc, errors)(inferredIdent.name, inferredKindAnnotation, typ)
            T.Declaration(
              decl.meta.typed.withDiagnostics(errors),
              T.Declaration.TypeAlias(inferredIdent, inferredKindAnnotation, inferredRHS),
              modifiers
            )
        }
      case N.Declaration.Error() =>
        T.Declaration(
          decl.meta.typed,
          T.Declaration.Error(),
          modifiers
        )
    }
    result
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
        ErrorType
      ) => false
      case Constructor(sym, _) =>
        sym.id == symbol.id
      case Func(from, to) =>
        occursIn(symbol, to) || from.exists(t => occursIn(symbol, t._2))
      case Forall(_, typ) =>
        occursIn(symbol, typ)
      case TApplication(f, arg) =>
        occursIn(symbol, f) || occursIn(symbol, arg)
      case ExistentialInstance(_, _) => false
      case Module(_, values) =>
        values.values.exists(t => occursIn(symbol, t))
      case Abstract(_, values, inner) =>
        values.keys.exists(_ == symbol) || occursIn(symbol, inner)
      case Uninferred => false
    }
  }

  private def assignExprToPattern(pattern: N.Pattern, expr: N.Expr): (T.Pattern, T.Expr) = {
    pattern.variant match {
      case N.Pattern.Annotated(pat, annotation) =>
        val inferredAnnotation = checkTypeAnnotation(annotation, Kind.Star)
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
        val inferredExpr = getInferredExpr(expr)
        val checkedPattern = checkPattern(pattern, inferredExpr.typ)
        (checkedPattern, inferredExpr)
    }
  }

  def getInferredExpr(expr: Named.Expr): T.Expr = {
    _checkedExprs.get(expr.meta.id).flatMap(_.get) match {
      case Some(e) => e
      case None =>
        val inferred = inferExpr(expr)
        _checkedExprs.update(expr.meta.id, WeakReference(inferred))
        inferred
    }
  }

  private def getTypeOfIdent(ident: N.Ident): (Type, Iterable[Diagnostic]) = {
    ctx.getTypeOfSymbol(ident.name) match {
      case Some(t) => (t, List.empty)
      case _ =>
        ident.getScope.resolveEntry(ident.name.text) match {
          case Some(ScopeEntry(_, Some(pos))) if ident.loc.start < pos =>
            (Uninferred, List(
              Diagnostic(
                loc = ident.loc,
                severity = Severity.Error,
                variant = UseBeforeAssignment(ident.name.text)
              )
            ))
          case _ =>
            val namedDecl = ctx.getDeclOfSymbol(ident.name)
            namedDecl.map(inferDeclaration)
            (ctx.getTypeOfSymbol(ident.name).getOrElse(ErrorType), List.empty)
        }
    }
  }

  def inferExpr(expr: N.Expr): T.Expr = {
    def makeTyped(variant: T.Expr.Variant, typ: Type) =
      T.Expr(expr.meta.typed, typ, variant)

    expr.variant match {
      case N.Expr.Var(ident) =>
        val inferredIdent = inferIdent(ident)
        val (typ, errors) = getTypeOfIdent(ident)
        T.Expr(
          meta = expr.meta.withDiagnostics(errors).typed,
          typ = typ,
          variant = T.Expr.Var(inferredIdent)
        )
      case mod@N.Expr.Module(_, _) =>
        inferModule(expr, mod)
      case N.Expr.Literal(N.Expr.LInt(value)) =>
        makeTyped(
          typ = Primitive.Int,
          variant = T.Expr.Literal(T.Expr.LInt(value))
        )
      case func@N.Expr.Func(_, _, _, _, _, _) =>
        inferFunc(expr, func)
      case call@N.Expr.Call(_, _, _) =>
        inferCall(expr, call)
      case N.Expr.Prop(e, prop) =>
        val inferredExpr = getInferredExpr(e)
        inferredExpr.typ match {
          case mod@Module(_, _) =>
            mod.typeOfString.get(prop) match {
              case Some(t) =>
                T.Expr(
                  meta = expr.meta.typed,
                  typ = t,
                  variant = T.Expr.Prop(inferredExpr, prop)
                )
              case None =>
                T.Expr(
                  meta = expr.meta.typed.withDiagnostic(
                    Diagnostic(
                      loc = Loc.between(e, expr),
                      severity = Severity.Error,
                      variant = NoSuchValueProperty(prop)
                    )
                  ),
                  typ = Uninferred,
                  variant = T.Expr.Prop(
                    inferredExpr, prop
                  )
                )
            }
          case _ =>
            T.Expr(
              meta = expr.meta.typed.withDiagnostic(
                Diagnostic(
                  loc = Loc.between(e, expr),
                  severity = Severity.Error,
                  variant = NotAModule
                )
              ),
              typ = Uninferred,
              variant = T.Expr.Prop(
                inferredExpr,
                prop
              )
            )
        }
      case w@N.Expr.WithExpression(_, _) =>
        inferWith(expr, w)
      case N.Expr.Error() =>
        makeTyped(
          typ = ErrorType,
          variant = T.Expr.Error()
        )
    }
  }

  private def getDeclBinders(decl: T.Declaration): Iterable[Symbol] = {
    decl.variant match {
      case T.Declaration.Let(pattern, _) => getPatternLabel(pattern)
        .map({ List(_) }).getOrElse(Nil)
      case T.Declaration.TypeAlias(name, _, _) => List(name.name)
      case T.Declaration.Include(_) => Nil
      case T.Declaration.Error() => Nil
    }
  }

  private def inferModule(expr: N.Expr, module: N.Expr.Module): T.Expr = {
    val inferredDecls = module.declarations.map(inferDeclaration)
    val abstractNames = inferredDecls
      .filter(_.isAbstract).flatMap(getDeclBinders)
      .toSet
    val types = module.scope.typeSymbols.values
      .filter(t => !(abstractNames contains t.symbol))
      .map(t => t.symbol -> ctx.getKindOfSymbol(t.symbol).getOrElse(Kind.Star))
      .toMap
    val values = module.scope.symbols.values
      .filter(t => !(abstractNames contains t.symbol))
      .map(e => e.symbol -> ctx.getTypeOfSymbol(e.symbol).getOrElse(Uninferred))
      .toMap
    val modTyp = Module(types, values)
    val typ = if (abstractNames.isEmpty) {
      modTyp
    } else {
      val abstractTypes = module.scope.typeSymbols.values
        .filter(abstractNames contains _.symbol)
        .map(t => t.symbol -> ctx.getKindOfSymbol(t.symbol).getOrElse(Kind.Star))
        .toMap
      val abstractValues = module.scope.symbols.values
          .filter(abstractNames contains _.symbol)
          .map(e => e.symbol ->
            ctx.getTypeOfSymbol(e.symbol).getOrElse(Uninferred))
          .toMap
      Abstract(
        abstractTypes,
        abstractValues,
        modTyp
      )
    }
    T.Expr(
      meta = expr.meta.typed,
      typ = typ,
      variant = T.Expr.Module(module.scope, inferredDecls)
    )
  }

  private def inferWith(
    e: Named.Expr, variant: Named.Expr.WithExpression
  ): T.Expr = {
    val e1 = variant.e1
    val e2 = variant.e2
    val inferredE1 = inferExpr(e1)
    val inferredE2 = inferExpr(e2)
    val (errors, typ) = inferWithHelper(inferredE1.typ, inferredE2.typ, e1.meta, e2.meta)
    T.Expr(
      meta = e.meta.typed.withDiagnostics(errors),
      typ = typ,
      variant = T.Expr.WithExpression(inferredE1, inferredE2)
    )
  }

  private def inferWithHelper(
    t1: Type, t2: Type,
    meta1: N.Meta, meta2: N.Meta,
    abstractTypes: Map[String, (Kind, Symbol)] = Map.empty,
    abstractValues: Map[String, (Type, Symbol)] = Map.empty
  ): (Vector[Diagnostic], Type) = (t1, t2) match {
      case (Module(types1, values1), Module(types2, values2)) =>
        val errors = ListBuffer.empty[Diagnostic]
        val types2Str = types2.map(t => t._1.text -> (t._1, t._2))
        val values2Str = values2.map(t => t._1.text -> t._1)
        for (typeSymbol1 <- types1.keySet) {
          types2Str.get(typeSymbol1.text) match {
            case Some((s, _)) =>
              val loc = ctx.getDeclOfTypeSymbol(s).map(_.loc).getOrElse(meta2.loc)
              errors.append(
                Diagnostic(
                  loc = loc,
                  severity = Severity.Error,
                  variant = ConflictingDecls(typeSymbol1.text)
                )
              )
            case None => ()
          }
        }
        for (symbol1 <- values1.keySet) {
          values2Str.get(symbol1.text) match {
            case Some(sym) =>
              val loc = ctx.getDeclOfSymbol(sym).map(_.loc).getOrElse(meta2.loc)
              errors.append(
                Diagnostic(
                  loc = loc,
                  severity = Severity.Error,
                  variant = ConflictingDecls(symbol1.text)
                )
              )
            case None => ()
          }
        }
        val missingAbsTypes = mutable.HashMap.empty[Symbol, Kind]
        val missingAbsValues = mutable.HashMap.empty[Symbol, Type]

        val abstractTypSubst = mutable.HashMap.empty[Symbol, Type]
        for ((absTyName, (absTypKind, absTypSymbol)) <- abstractTypes) {
          types2Str.get(absTyName) match {
            case None => missingAbsTypes.update(absTypSymbol, absTypKind)
            case Some((sym, kind)) =>
              abstractTypSubst.put(absTypSymbol, Var(sym))
              if (kind != absTypKind) {
                errors.append(
                  Diagnostic(
                    loc = ctx.getDeclOfTypeSymbol(sym).map(_.loc).getOrElse(meta2.loc),
                    severity = Severity.Error,
                    variant = InvalidTypeOverride(absTypKind, kind)
                  )
                )
              }
          }
        }

        for ((absValName, (absValTyp, absValSymbol)) <- abstractValues) {
          values2Str.get(absValName) match {
            case None => missingAbsValues.update(absValSymbol, absValTyp)
            case Some(symbol) =>
              val substAbstractTyp = applySubst(absValTyp, abstractTypSubst)
              val symTyp = applySubst(
                ctx.getTypeOfSymbol(symbol).getOrElse(Uninferred),
                abstractTypSubst
              )
              val meta = ctx.getDeclOfSymbol(symbol).map(_.meta).getOrElse(meta2)
              val errs = assertSubType(meta, exact = true)(symTyp, substAbstractTyp)
              if (errs.nonEmpty) {
                errors.append(
                  Diagnostic(
                    loc = meta.loc,
                    severity = Severity.Error,
                    variant = InvalidValueOverride(substAbstractTyp, symTyp)
                  )
                )
                errors.appendAll(errs)
              }
          }
        }
        val modTyp = Module(types1 ++ types2, values1 ++ values2)
        val typ = if (missingAbsTypes.isEmpty && missingAbsValues.isEmpty)
          modTyp
        else
          Abstract(
            missingAbsTypes.toMap,
            missingAbsValues.toMap,
            modTyp
          )
        (errors.toVector, typ)
      case (Abstract(absTypes, absValues, innerType), _) =>
        inferWithHelper(
          innerType, t2, meta1, meta2,
          absTypes.map(entry => entry._1.text -> (entry._2, entry._1)),
          absValues.map(entry => entry._1.text -> (entry._2, entry._1))
        )
      case _ =>
        val errors = ListBuffer.empty[Diagnostic]
        t1 match {
          case Module(_, _) => ()
          case _ =>
            errors.append(Diagnostic(
              loc = meta1.loc,
              severity = Severity.Error,
              variant = NotAModule
            ))
        }
        t2 match {
          case Module(_, _) => ()
          case _ =>
            errors.append(Diagnostic(
              loc = meta2.loc,
              severity = Severity.Error,
              variant = NotAModule
            ))
        }
        (errors.toVector, ErrorType)
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
    val inferredFunc = getInferredExpr(call.func)
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
              var inferredExpr = getInferredExpr(arg.value)
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
              var inferredExpr = getInferredExpr(expr)
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
        Constructor(_, _)
        | ErrorType
        | Uninferred
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
      case Module(types, values) =>
        Module(types, values.mapValues(applyEnv))
      case Abstract(types, values, inner) =>
        Abstract(
          types,
          values.mapValues(applyEnv),
          applyEnv(inner)
        )
      case Forall(params, inner) =>
        Forall(
          params,
          applyEnv(inner)
        )
    }
  }

  private def inferArgs(args: Vector[Named.Expr.Arg]): Vector[T.Expr.Arg] = {
    args.map(arg => {
      T.Expr.Arg(label = arg.label, value = getInferredExpr(arg.value))
    })
  }

  private def inferFunc(expr: N.Expr, func: N.Expr.Func): T.Expr = {
    Debug.log(s"inferFunc($func)")
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
        val checkedRetTypAnnotation = checkTypeAnnotation(t, Kind.Star)
        val expectedRetTyp = annotationToType(checkedRetTypAnnotation)
        (checkExpr(func.body, expectedRetTyp), Some(checkedRetTypAnnotation))
      case None =>
        (getInferredExpr(func.body), None)
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
          val inferredAnnotation = checkTypeAnnotation(annotation, Kind.Star)
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

  private def checkExpr(expr: N.Expr, typ: Type): T.Expr = {
    (expr.variant, typ) match {
      case (
        N.Expr.Error()
        | N.Expr.Call(_, _, _)
        | N.Expr.Literal(_)
        | N.Expr.Var(_),
        _
      )=>
        val inferredExpr = getInferredExpr(expr)
        val errors = assertSubType(expr.meta)(inferredExpr.typ, typ)
        inferredExpr.copy(
          meta = inferredExpr.meta.withDiagnostics(errors)
        )
      case (func@N.Expr.Func(_, _, _, _, _, _), _) =>
        checkFunc(expr, func, typ)
      case (_, Forall(_, innerTyp)) =>
        checkExpr(expr, innerTyp)
      case (_, _) =>
        val inferred = getInferredExpr(expr)
        val errors = assertSubType(expr.meta)(inferred.typ, typ)
        inferred.copy(
          meta = inferred.meta.withDiagnostics(errors),
          typ
        )
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
        val checkedRetTypAnnotation = func
          .returnTypeAnnotation
          .map({ checkAnnotation(_, to) })
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
        val e = getInferredExpr(expr)
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
                  N.Declaration.TypeAlias(_, _, _),
                  _
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
    _assertSubType(meta, exact, ListBuffer.empty)(a, b).toVector
  }

  private def _assertSubType(
    meta: N.Meta, exact: Boolean = false,
    errors: ListBuffer[Diagnostic]
  )(_a: Type, _b: Type): ListBuffer[Diagnostic] = {
    val a = resolveForwardAlias(meta, resolveType(_a))
    val b = resolveForwardAlias(meta, resolveType(_b))

    Debug.log(s"checking if $a <: $b (${_a} <: ${_b})")

    (a, b) match {
      case (Constructor(a1, _), Constructor(b1, _)) if a1.id == b1.id
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
      case (_, Forall(param, innerTyp)) =>
        val subst = mutable.Map.empty[Symbol, Type]
        subst.put(param, ctx.makeGenericType(param.text))
        val substituted = applySubst(innerTyp, subst)
        _assertSubType(meta, exact, errors)(a, substituted)
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

  private def instantiateL(meta: N.Meta, errors: ListBuffer[Diagnostic])(existential: ExistentialInstance, t: Type): Unit = {
    ctx.getGeneric(existential.id) match {
      case Some(a) =>
        _assertSubType(meta, errors = errors)(a, t)
      case None =>
        ctx.assignGeneric(existential.id, t)
    }
  }

  private def instantiateR(meta: N.Meta, errors: ListBuffer[Diagnostic])
    (a: Type, existential: ExistentialInstance) = {
    ctx.getGeneric(existential.id) match {
      case Some(b) =>
        _assertSubType(meta, errors = errors)(a, b)
      case None =>
        ctx.assignGeneric(existential.id, a)
    }
  }

  private def applySubst(t: Type, subst: collection.Map[Symbol, Type]): Type = {
    t match {
      case (
        Constructor(_, _)
        | ErrorType
        | ExistentialInstance(_, _)
        | Uninferred
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
      case Module(types, values) =>
        Module(types, values.mapValues((typ) => applySubst(typ, subst)))
      case Abstract(types, values, inner) =>
        Abstract(
          types,
          values.mapValues(t => applySubst(t, subst)),
          applySubst(inner, subst)
        )
      case Func(from, to) =>
        Func(
          from.map(param => (param._1, applySubst(param._2, subst))),
          applySubst(to, subst)
        )
    }
  }

  private def checkPattern(
    pattern: N.Pattern, typ: Type
  ): T.Pattern = {
    Debug.log(s"checkPattern($pattern, $typ)")
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
//    (ident.duplicateBinder, ctx.getTypeOfSymbol(ident.name)) match {
//      case (false, Some(t)) =>
//        throw new Error(
//          s"Compiler bug: Tried to assign type to already assigned ident $ident".concat(
//            s" (loc: ${ident.loc}, old type: $t; new type: $typ)"
//          )
//        )
//      case _ => ()
//    }
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

  private def inferTypeAnnotation(annotation: N.TypeAnnotation): T.TypeAnnotation = {
    val (variant: T.TypeAnnotation.Variant, kind: Kind) = annotation.variant match {
      case N.TypeAnnotation.Var(ident) =>
        val inferredIndent = inferIdent(ident)
        val k = ctx.getKindOfSymbol(inferredIndent.name).getOrElse(Kind.Star)
        (T.TypeAnnotation.Var(inferredIndent), k)
      case N.TypeAnnotation.Forall(scope, params, innerAnnotation) =>
        val inferredParams = inferGenericParams(params.toVector)
        val inferredInner = inferTypeAnnotation(innerAnnotation)
        (
          T.TypeAnnotation.Forall(scope, inferredParams, inferredInner),
          inferredInner.kind
        )
      case N.TypeAnnotation.Func(params, retType) =>
        val inferredParams = params.map(param => {
          (param._1.map(inferIdent), checkTypeAnnotation(param._2, Kind.Star))
        })
        val checkedRetTypAnnotation = checkTypeAnnotation(retType, Kind.Star)
        (
          T.TypeAnnotation.Func(
            inferredParams,
            checkedRetTypAnnotation
          ),
          Kind.Star
        )
      case N.TypeAnnotation.Prop(e, prop) =>
        val inferredExpr = getInferredExpr(e)
        inferredExpr.typ match {
          case m@Module(_, _) =>
            m.kindOfString.get(prop) match {
              case Some(kind) =>
                (
                  T.TypeAnnotation.Prop(
                    inferredExpr,
                    prop
                  ),
                  kind
                )
              case None =>
                (
                  T.TypeAnnotation.Prop(
                    inferredExpr.copy(
                      meta = inferredExpr.meta.withDiagnostic(
                        Diagnostic(
                          loc = e.loc,
                          severity = Severity.Error,
                          variant = NoSuchTypeProperty(prop)
                        )
                      )
                    ),
                    prop
                  ),
                  Kind.Star
                )
            }
          case _ =>
            (
              T.TypeAnnotation.Prop(
                inferredExpr.copy(
                  meta = inferredExpr.meta.withDiagnostic(
                    Diagnostic(
                      loc = e.loc,
                      severity = Severity.Error,
                      variant = NotAModule
                    )
                  )
                ),
                prop
              ),
              Kind.Star
            )
        }
      case N.TypeAnnotation.TApplication(f, args) =>
        val inferredF = inferTypeAnnotation(f)
        val expectedKindArgs = getKindArgs(inferredF.kind).toVector
        val checkedArgs = checkTypeArgs(args, expectedKindArgs)
        val kind = getKindOfTypeApplication(inferredF.kind, expectedKindArgs.length)
        (
          T.TypeAnnotation.TApplication(
            inferredF,
            checkedArgs
          ),
          kind
        )
      case N.TypeAnnotation.Error =>
        (T.TypeAnnotation.Error, Kind.Star)
    }
    T.TypeAnnotation(
      annotation.meta.typed,
      kind,
      variant
    )
  }

  private def checkTypeAnnotation(
    annotation: N.TypeAnnotation,
    expectedKind: Kind
  ): T.TypeAnnotation = {
    val inferredAnnotation = inferTypeAnnotation(annotation)
    inferredAnnotation.copy(
      meta = inferredAnnotation.meta.withDiagnostics(
        assertKindMatch(annotation.loc, expectedKind, inferredAnnotation.kind))
    )
  }

  private def checkTypeArgs(args: Iterable[N.TypeAnnotation], expectedKindArgs: Vector[Kind]) =
    // args upto expected args length
    args.take(expectedKindArgs.length)
      .zip(expectedKindArgs)
      .map({ case (arg, kind) => checkTypeAnnotation(arg, kind) }) ++
    // args after expected args length; should have ExtraTypeArg error
    args.drop(expectedKindArgs.length)
      .map(arg => {
        val inferredArg = inferTypeAnnotation(arg)
        inferredArg.copy(
          meta = inferredArg.meta.withDiagnostic(
            Diagnostic(
              loc = arg.loc,
              severity = Severity.Error,
              variant = ExtraTypeArg
            )
          )
        )
      })

  @tailrec def getKindOfTypeApplication(kind: Kind, argsLength: Int): Kind = {
    if (argsLength == 0) {
      kind
    } else {
      kind match {
        case Kind.Star => Kind.Star
        case Kind.KFun(_, a) =>
          getKindOfTypeApplication(a, argsLength - 1)
      }
    }
  }

  def getKindArgs(kind: Kind): List[Kind] = {
    kind match {
      case Kind.Star => Nil
      case Kind.KFun(from, to) =>
        from::getKindArgs(to)
    }
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

  private def checkAnnotation(
    annotation: N.TypeAnnotation, typ: Type
  ): T.TypeAnnotation = {
    val inferredAnnotation = checkTypeAnnotation(annotation, Kind.Star)
    val annotTyp = annotationToType(inferredAnnotation)
    val errors = assertSubType(annotation.meta)(annotTyp, typ)
    inferredAnnotation.copy(
      meta = inferredAnnotation.meta.withDiagnostics(errors)
    )
  }

  private def assertKindMatch(
    loc: Loc, expectedKind: Kind, kind: Kind
  ): List[Diagnostic] = {
    if (expectedKind != kind) {
      List(
        Diagnostic(
          loc = loc,
          severity = Severity.Error,
          variant = KindMismatch(expectedKind, kind)
        )
      )
    } else {
      List.empty
    }
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
      case T.TypeAnnotation.Prop(e, prop) =>
        resolveType(e.typ) match {
          case modTyp@Module(_, _) =>
            modTyp
              .symbolOfString
              .get(prop)
              .map(sym => Var(sym))
              .getOrElse(ErrorType)
          case _ =>
            Uninferred
        }
      case T.TypeAnnotation.Error =>
        ErrorType
    })
  }
}
