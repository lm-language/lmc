package lmc

import scala.collection.mutable
import lmc.syntax._
import lmc.common.{Loc, Symbol}
import lmc.diagnostics._
import lmc.types._

import scala.collection.mutable.ListBuffer

final class TypeChecker(
  private val ctx: Context.TC
) {
  import lmc.syntax.{Named => N, Typed => T}

  def checkSourceFile(parsed: Parsed.SourceFile): T.SourceFile = {
    val named = new Renamer(ctx).renameSourceFile(parsed)
    val declarations = checkModule(parsed.loc, named.declarations)
    T.SourceFile(
      meta = named.meta.typed,
      declarations = declarations,
      scope = named.scope
    )
  }

  private def checkModule(loc: Loc, declarations: Iterable[N.Declaration]): Iterable[T.Declaration] = {
    declarations.map(inferDeclaration)
  }

  private def inferDeclaration(declaration: N.Declaration): T.Declaration = {
    val variant = declaration.variant match {
      case N.Declaration.Let(pattern, expr) =>
        val (checkedPattern, checkedExpr) = bindExprToPattern(pattern, expr)
        T.Declaration.Let(pattern = checkedPattern, rhs = checkedExpr)
      case N.Declaration.Extern(ident, annotation) =>
        val checkedAnnotation = checkAnnotation(annotation)
        val checkedIdent = this.identToTypedIdent(ident)
        bindTypeToIdent(ident, annotationToType(checkedAnnotation))
        T.Declaration.Extern(checkedIdent, checkedAnnotation)
      case N.Declaration.Error() =>
        T.Declaration.Error()
    }
    T.Declaration(meta = declaration.meta.typed, variant = variant)
  }

  private def inferExpr(expr: N.Expr): T.Expr = {
    import Named.{Expr => NE}
    import Typed.{Expr => TE}
    val (variant: T.Expr.Variant, typ, diagnostics):
        (T.Expr.Variant, Type, Iterable[Diagnostic]) = expr.variant match {
      case NE.Literal(NE.LInt(l)) =>
       (TE.Literal(TE.LInt(l)), Primitive.Int, List())
      case NE.Var(ident) =>
        val typ = ctx.getTypeOfSymbol(ident.name) match {
          case Some(t) => t
          case None =>
            ctx.setTypeOfSymbol(ident.name, ErrorType)
            ErrorType
        }
        (TE.Var(T.Ident(ident.meta.typed, ident.name)), typ, List.empty)
      case (NE.Func(tok, scope, genericParams, params, returnTypeAnnotation, body)) =>
        var positionalArgTypes = Vector.empty[Func.Param]
        var labeledArgTypes = Vector.empty[Func.Param]

        var positionalDone = false
        val checkedGenericParams = genericParams.map(inferGenericParam)
        val checkedParams = params.map((param) => {
          val checkedPattern = inferPattern(param.pattern)
          var checkedParam = TE.Param(checkedPattern)
          getParamLabel(checkedParam) match {
            case Some(symbol) =>
              positionalDone = true
              labeledArgTypes = labeledArgTypes :+ (Some(symbol), checkedParam.pattern.typ)
            case None =>
              if (positionalDone) {
                checkedParam = checkedParam.copy(
                  pattern = checkedParam.pattern.copy(
                    meta = checkedParam.pattern.meta.withDiagnostic(
                      Diagnostic(
                        loc = checkedParam.pattern.loc,
                        severity = Severity.Error,
                        variant = PositionalParamAfterLabelled
                      )
                    )
                  )
                )
              }
              positionalArgTypes = positionalArgTypes :+ (None, checkedParam.pattern.typ)
          }
          checkedParam
        })

        val typedBody = returnTypeAnnotation match {
          case Some(annot) =>
            val annotatedType = annotationToType(checkAnnotation(annot))
            checkExpr(body, annotatedType)
          case None =>
            inferExpr(body)
        }
        val typedVariant: TE.Variant = TE.Func(
          tok, scope,
          checkedGenericParams,
          checkedParams, None, typedBody)

        val typeFrom = positionalArgTypes ++ labeledArgTypes
        val typ: Type = lmc.types.Func(from = typeFrom, to = typedBody.typ)
        (
          typedVariant,
          typ,
          List.empty
        )
      case call@N.Expr.Call(_, _, _) =>
       inferCall(call)
      case NE.Error() => (TE.Error(), ErrorType, List.empty)
    }
    T.Expr(
      meta =
        expr.meta.copy(
          diagnostics = expr.meta.diagnostics ++ diagnostics
        ).typed,
      typ = typ,
      variant = variant
    )
  }

  private def inferCall(call: Named.Expr.Call): (T.Expr.Call, Type, Iterable[Diagnostic]) = {
    var inferredFunc = inferExpr(call.func)
    val errors = ListBuffer.empty[Diagnostic]
    val (args, retTyp) = inferredFunc.typ match {
      case Func(from, to) =>
        val args = checkArgs(call.argsLoc, errors)(call.args, from)
        (args, to)
      case typ =>
        inferredFunc = inferredFunc.copy(
          meta = inferredFunc.meta.withDiagnostic(
            Diagnostic(
              loc = inferredFunc.loc,
              severity = Severity.Error,
              variant = diagnostics.NotAFunction(typ)
            )
          )
        )
        (call.args.map(inferArg(errors)), ErrorType)
    }
    (T.Expr.Call(call.argsLoc, inferredFunc, args), retTyp, errors)
  }

  private def inferArg(errors: ListBuffer[Diagnostic])(arg: N.Expr.Arg): T.Expr.Arg = {
    T.Expr.Arg(arg.label, inferExpr(arg.value))
  }

  private def checkArgs(argsLoc: Loc, errors: ListBuffer[Diagnostic])
    (namedArgs: Vector[N.Expr.Arg], paramTypes: Vector[Func.Param])
    : Vector[T.Expr.Arg] = {
    var result = Vector.empty[T.Expr.Arg]
    var i = 0
    val missingArgs = ListBuffer.empty[(Option[String], Type)]

    val labeledParams = mutable.Map.empty[String, Type]
    val labeledArgs = mutable.Map.empty[String, (token.Token, N.Expr)]
    var labeledArgEncountered = false
    while (i < paramTypes.length) {
      val (paramLabel, paramType) = paramTypes(i)

      paramLabel match {
        case Some(symbol) =>
          labeledParams += symbol.text -> paramType
        case None =>
          ()
      }
      if (i >= namedArgs.length) {
        paramLabel match {
          case Some(symbol) =>
            // ignore labelled params for later because
            // they can appear later in the arg list
            ()
          case None =>
            // non labelled param which doesn't occur
            // in arg list. Add error
            missingArgs.append(
              (paramLabel.map(_.text), paramType)
            )
        }

      } else {
        val arg = namedArgs(i)
        arg.label match {
          case Some((tok, name)) =>
            labeledArgEncountered = true
            // labeled arg; Can appear out of order so we can
            // add it to labeled args map for later checking
            labeledArgs += name -> (tok, arg.value)
          case None =>
            // non labelled argument should appear at exactly same
            // position as param; This can be checked with param type
            val typedArgValue = checkExpr(arg.value, paramType)

            // because this argument has been handled in a positional
            // way, we should remove it from the labelledParams map
            // so that it isn't checked during checking of labeled params
            paramLabel match {
              case Some(symbol) =>
                labeledParams -= symbol.text
              case None => ()
            }

            // also, this is a positional arg so it should appear
            // before any labeled args; check for that
            val typedArg = if (labeledArgEncountered) {
              T.Expr.Arg(None, typedArgValue.copy(
                meta = typedArgValue.meta.withDiagnostic(
                  Diagnostic(
                    loc = typedArgValue.loc,
                    severity = Severity.Error,
                    variant = PositionalArgAfterLabelled
                  )
                )
              ))
            } else {
              T.Expr.Arg(None, typedArgValue)
            }
            result = result :+ typedArg
        }
      }
      i += 1
    }

    for ((name, typ) <- labeledParams) {
      labeledArgs.get(name) match {
        case Some((tok, arg)) =>
          result = result :+
            T.Expr.Arg(
              Some(tok, name),
              checkExpr(arg, typ))
        case None =>
          missingArgs.append((Some(name), typ))
      }
    }

    for ((name, (tok, expr)) <- labeledArgs) {
      labeledParams.get(name) match {
        case Some(_) =>
          // this case should have already checked by
          // the previous loop which iterates over all labeled
          // params; Do nothing
          ()
        case None =>
          val inferredExpr = inferExpr(expr)
          errors.append(
            Diagnostic(
              loc = tok.loc,
              severity = Severity.Error,
              variant = NoSuchParamLabel(name)
            )
          )
          result = result :+
            T.Expr.Arg(
              Some(tok, name),
              inferredExpr
            )
      }
    }

    if (missingArgs.nonEmpty) {
      errors.append(Diagnostic(
        loc = argsLoc,
        severity = Severity.Error,
        variant = MissingArguments(missingArgs)
      ))
    }
    result
  }

  private def getParamLabel(param: Typed.Expr.Param): Option[Symbol] = {
    getPatternLabel(param.pattern)
  }

  def getPatternLabel(pattern: Typed.Pattern): Option[Symbol] = {
    pattern.variant match {
      case T.Pattern.Var(ident) =>
          Some(ident.name)
      case T.Pattern.Annotated(inner, _) =>
          getPatternLabel(inner)
      case _ => None
    }
  }

  private def inferPattern(pattern: N.Pattern): T.Pattern = {
    val (variant: T.Pattern.Variant, typ: Type, diagnostics: Iterable[Diagnostic]) =
      pattern.variant match {
        case N.Pattern.Var(ident) =>
          val checkedIdent = this.identToTypedIdent(ident)
          val variant = T.Pattern.Var(checkedIdent)
          ctx.getTypeOfSymbol(checkedIdent.name) match {
            case Some(typ) =>
              this.bindTypeToIdent(ident, typ)
              (variant, typ, List.empty)
            case None =>
              val typ = lmc.types.ErrorType
              this.bindTypeToIdent(ident, typ)
              (variant, typ, List(
                Diagnostic(
                  loc = ident.loc,
                  severity = Severity.Error,
                  variant = MissingTypeAnnotation
                )
              ))
          }
        case N.Pattern.Annotated(innerPattern, annotation) =>
          val checkedAnnotation: T.TypeAnnotation = checkAnnotation(annotation)
          val expectedType: lmc.types.Type = annotationToType(checkedAnnotation)
          val checkedInnerPattern: T.Pattern = checkPattern(innerPattern, expectedType)
          (T.Pattern.Annotated(
            checkedInnerPattern,
            checkedAnnotation
          ), expectedType, List())
        case N.Pattern.Error =>
          (T.Pattern.Error, lmc.types.ErrorType, List())
      }
    T.Pattern(
      meta = pattern.meta.copy(
        diagnostics = pattern.meta.diagnostics ++ diagnostics
      ).typed,
      typ = typ,
      variant = variant
    )
  }

  private def bindExprToPattern(pattern: N.Pattern, expr: N.Expr): (T.Pattern, T.Expr) = {
    val (variant: T.Pattern.Variant, checkedExpr: T.Expr,diagnostics) = pattern.variant match {
      case N.Pattern.Var(ident) =>
        val checkedExpr = inferExpr(expr)
        val typ = checkedExpr.typ
        val newIdent = bindTypeToIdent(ident, typ)
        (T.Pattern.Var(newIdent), checkedExpr, List())
      case N.Pattern.Annotated(p, annotation) =>
        val checkedAnnotation = checkAnnotation(annotation)
        val typ = annotationToType(checkedAnnotation)
        val checkedPattern = checkPattern(p, typ)
        val checkedExpr = checkExpr(expr, typ)
        (T.Pattern.Annotated(
          checkedPattern,
          checkedAnnotation
        ), checkedExpr, List())
      case N.Pattern.Error =>
        (T.Pattern.Error, inferExpr(expr), List())
    }
    (
      T.Pattern(
        meta =
          pattern.meta.copy(
            diagnostics = pattern.meta.diagnostics ++ diagnostics
          ).typed,
        typ = checkedExpr.typ,
        variant = variant
      ),
      checkedExpr
    )
  }

  private def checkPattern(pattern: N.Pattern, typ: Type): T.Pattern = {
    val (checkedVariant, diagnostics) = pattern.variant match {
      case N.Pattern.Var(ident) =>
        bindTypeToIdent(ident, typ)
        (
          T.Pattern.Var(T.Ident(ident.meta.typed, ident.name)),
          List.empty
        )
      case N.Pattern.Annotated(p, annotation) =>
        val checkedAnnotation = checkAnnotation(annotation)
        val annotationType = annotationToType(checkedAnnotation)
        val diagnostics =
          if (typeMoreGeneralThan(typ, annotationType)) {
             List.empty
          }
          else {
            List(
              Diagnostic(
                loc = pattern.loc,
                severity = Severity.Error,
                variant = TypeMismatch(typ, annotationType)
              )
            )
          }
        val innerPattern = checkPattern(p, annotationType)
        (
          T.Pattern.Annotated(innerPattern, checkedAnnotation),
          diagnostics
        )
      case N.Pattern.Error =>
        (T.Pattern.Error, List.empty)
    }
    T.Pattern(
      meta = pattern.meta.copy(
        diagnostics = pattern.meta.diagnostics ++ diagnostics
      ).typed,
      variant = checkedVariant,
      typ = typ
    )
  }

  private def checkExpr(expr: N.Expr, typ: Type): T.Expr = {
    val (variant: T.Expr.Variant, diagnostics: Iterable[Diagnostic]) = expr.variant match {
      case N.Expr.Literal(N.Expr.LInt(x)) =>
        val exprTyp = Primitive.Int
        val diagnostics = assertTypeMoreGeneralThan(expr.loc)(typ, exprTyp)
        (T.Expr.Literal(T.Expr.LInt(x)), diagnostics)
      case N.Expr.Var(ident) =>
        val varTyp = ctx.getTypeOfSymbol(ident.name) match {
          case Some(t) => t
          case None => ErrorType
        }
        val diagnostics = assertTypeMoreGeneralThan(loc = ident.loc)(typ, varTyp)
        (T.Expr.Var(ident = T.Ident(meta = ident.meta.typed, ident.name)), diagnostics)
      case N.Expr.Call(_, _, _) =>
        val inferredExpr = inferExpr(expr)
        if (typeMoreGeneralThan(typ, inferredExpr.typ)) {
          (inferredExpr.variant, List())
        } else {
          (inferredExpr.variant, List(
            Diagnostic(
              loc = inferredExpr.loc,
              severity = Severity.Error,
              variant = TypeMismatch(
                typ, inferredExpr.typ
              )
            )
          ))
        }
      case N.Expr.Func(tok, sc, genericParams, namedParams, retTyp, body) =>
        typ match {
          case Forall(expectedGenericParams, expectedTyp) =>
            val checkedGenericParams = checkGenericParams(
              genericParams,
              expectedGenericParams
            )
            val checkedInnerExpr = checkExpr(
              expr.copy(
                variant = N.Expr.Func(
                  tok, sc,
                  List(),
                  namedParams, retTyp, body
                )
              ),
              expectedTyp
            )
            ???
          case Func(expectedParamTypesWithLabels, expectedRetTyp) =>
            val errors = mutable.ListBuffer.empty[Diagnostic]
            val checkedGenericParams = checkGenericParams(genericParams, List())

            val checkedParams = checkParamList(tok.loc, errors)(
              expectedParamTypesWithLabels.map(_._2),
              namedParams
            )
            var checkedRetTyp: Option[Type] = None
            val checkedRetTypeAnnotation = retTyp.map((annot) => {
              val checkedAnnot: T.TypeAnnotation = this.checkAnnotation(annot)
              val typ: Type = this.annotationToType(checkedAnnot)
              checkedRetTyp = Some(typ)
              val errs = if (!typeMoreGeneralThan(expectedRetTyp, typ)) {
                List(Diagnostic(
                    loc = annot.loc,
                    severity = Severity.Error,
                    variant = TypeMismatch(
                      expectedRetTyp,
                      typ
                    )
                ))
              } else {
                List.empty
              }
              checkedAnnot.copy(
                meta = checkedAnnot.meta.copy(
                  diagnostics = checkedAnnot.meta.diagnostics ++ errs
                )
              )
            })
            val checkedBody = checkExpr(body, checkedRetTyp.getOrElse(expectedRetTyp))
            (T.Expr.Func(
              tok,
              sc,
              checkedGenericParams,
              checkedParams,
              checkedRetTypeAnnotation,
              checkedBody
            ), errors)
          case expectedTyp =>
            val inferredExpr = inferExpr(expr)
            (
              inferredExpr.variant,
              List(Diagnostic(
                loc = expr.loc,
                severity = Severity.Error,
                variant = TypeMismatch(expectedTyp, inferredExpr.typ)
              ))
            )
        }
      case N.Expr.Error() =>
        (T.Expr.Error(), List.empty)
    }
    T.Expr(
      meta = expr.meta.copy(
        diagnostics = expr.meta.diagnostics ++ diagnostics
      ).typed,
      variant = variant,
      typ = typ
    )
  }

  private def checkGenericParams(params: Iterable[N.GenericParam],
    expectedParams: Iterable[Symbol]): Iterable[T.GenericParam] = {
    val paramsVect = params.toVector
    val expectedParamsVect = expectedParams.toVector
    val checkedParams = ListBuffer.empty[T.GenericParam]
    var i = 0
    while (i < expectedParamsVect.length) {
      i += 1
    }
    while (i < paramsVect.length) {
      i += 1
    }
    checkedParams
  }

  private def checkParamList(fnTokenLoc: Loc, errors: mutable.ListBuffer[Diagnostic])
    (expectedTypes: Vector[Type], namedParams: Iterable[N.Expr.Param]): Vector[T.Expr.Param] = {
    val namedParamsArray = namedParams.toArray
    val typedParams = ListBuffer.empty[T.Expr.Param]
    var i = 0
    while (i < expectedTypes.length) {
      if (i < namedParamsArray.length) {
        val expectedType = expectedTypes(i)
        val namedParam = namedParamsArray(i)
        val typedPattern = checkPattern(namedParam.pattern, expectedType)
        val typedParam = T.Expr.Param(pattern = typedPattern)
        typedParams.append(typedParam)
      }
      i += 1
    }
    while (i < namedParamsArray.length) {
      val namedParam = namedParamsArray(i)
      val typedPattern =
        inferPattern(namedParam.pattern)
      val typedParam = T.Expr.Param(typedPattern.copy(meta = typedPattern.meta.withDiagnostic(
        Diagnostic(
          loc = typedPattern.loc,
          severity = Severity.Error,
          variant = diagnostics.ExtraParam
        )
      )))

      typedParams.append(typedParam)

      i += 1
    }
    typedParams.toVector
  }

  private def assertTypeMoreGeneralThan(loc: Loc)(t1: Type,  t2: Type): Iterable[Diagnostic] = {
    if (typeMoreGeneralThan(t1, t2)) {
      List.empty
    } else {
      List(
        Diagnostic(
          loc = loc,
          severity = Severity.Error,
          variant = TypeMismatch(t1, t2)
        )
      )
    }
  }

  private def typeMoreGeneralThan(t1: Type, t2: Type): Boolean = {
    val t1Instance = instantiate(t1)
    t1Instance == t2
  }

  private def instantiate(typ: Type): Type = {
    typ
  }

  private def identToTypedIdent(ident: N.Ident): T.Ident = {
    T.Ident(meta = ident.meta.typed, ident.name)
  }

  private def checkAnnotation(annotation: Named.TypeAnnotation): T.TypeAnnotation = {
    val typedVariant = annotation.variant match {
      case N.TypeAnnotation.Var(ident) =>
        T.TypeAnnotation.Var(
          T.Ident(meta = ident.meta.typed, ident.name)
        )
      case N.TypeAnnotation.Func(params, typ) =>
        T.TypeAnnotation.Func(
          params.map((param) => {
            val (label, annotation) = param
            val checkedAnnotation = this.checkAnnotation(annotation)
            val checkedLabel = label.map(identToTypedIdent)
            (checkedLabel, checkedAnnotation)
          }),
          this.checkAnnotation(typ)
        )
      case N.TypeAnnotation.Forall(scope, params, typ) =>
        val typedParams = params.map(inferGenericParam)
        val checkedTyp = checkAnnotation(typ)
        T.TypeAnnotation.Forall(scope, typedParams, checkedTyp)
      case N.TypeAnnotation.Error =>
        T.TypeAnnotation.Error
    }
    T.TypeAnnotation(
      meta = annotation.meta.typed,
      variant = typedVariant
    )
  }

  private def inferGenericParam(param: N.GenericParam): T.GenericParam = {
    val checkedAnnotation = param.kindAnnotation.map(checkKindAnnotation)
    val kind = checkedAnnotation.map(kindAnnotationToKind)
    kind match {
      case Some(k) =>
        ctx.setKindOfSymbol(param.ident.name, k)
      case None => ()
    }
    ctx.setTypeVar(
      param.ident.name,
      Generic(param.ident.name)
    )
    T.GenericParam(
      meta = param.meta.typed,
      ident = T.Ident(
        param.ident.meta.typed,
        param.ident.name
      ),
      kindAnnotation = checkedAnnotation
    )
  }

  private def kindAnnotationToKind(annotation: Typed.KindAnnotation) = {
    annotation.variant match {
      case T.KindAnnotation.Star => Kind.Star
    }
  }

  private def checkKindAnnotation(annotation: N.KindAnnotation): T.KindAnnotation = {
    annotation.asInstanceOf[T.KindAnnotation]
  }


  private def annotationToType(annotation: T.TypeAnnotation): Type = {
    annotation.variant match {
      case T.TypeAnnotation.Var(ident) =>
        ctx.getTypeVar(ident.name) match {
          case Some(t) => t
          case None =>
            Var(ident.name)
        }
      case T.TypeAnnotation.Func(params, ret) => {
        val paramTypes = params.map((param) => {
          val (label, annot) = param
          (label.map(_.name), annotationToType(annot))
        })
        val retTyp = annotationToType(ret)
        lmc.types.Func(paramTypes.toVector, retTyp)
      }
      case T.TypeAnnotation.Forall(_, params, ret) => {
        val paramTypes = params.map(_.ident.name)
        val retType = annotationToType(ret)
        lmc.types.Forall(paramTypes, retType)
      }
      case T.TypeAnnotation.Error => ErrorType
    }
  }

  private def bindTypeToIdent(ident: N.Ident, typ: Type): T.Ident = {
    ctx.setTypeOfSymbol(ident.name, typ)
    T.Ident(ident.meta.typed, ident.name)
  }
}