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
    decl.variant match {
      case N.Declaration.Let(pattern, rhs) =>
        val (checkedPattern, checkedRhs) = assignExprToPattern(pattern, rhs)
        T.Declaration(
          decl.meta.typed,
          T.Declaration.Let(
            checkedPattern, checkedRhs
          )
        )
      case N.Declaration.Extern(ident, annotation) =>
        val inferredAnnotation = inferAnnotation(annotation)
        val typ = annotationToType(inferredAnnotation)
        val inferredIdent = checkIdent(ident, typ)
        T.Declaration(
          decl.meta.typed,
          T.Declaration.Extern(inferredIdent, inferredAnnotation)
        )
      case N.Declaration.Error() =>
        T.Declaration(
          decl.meta.typed,
          T.Declaration.Error()
        )
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

  private def inferCall(expr: N.Expr, call: N.Expr.Call): T.Expr = {
    val inferredFunc = inferExpr(call.func)
    inferredFunc.typ match {
      case _ =>
        val inferredArgs = inferArgs(call.args)
        T.Expr(
          meta = expr.meta.withDiagnostic(
            Diagnostic(
              loc = expr.loc,
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
  }

  private def inferArgs(args: Vector[Named.Expr.Arg]): Vector[T.Expr.Arg] = {
    args.map(arg => {
      T.Expr.Arg(label = arg.label, value = inferExpr(arg.value))
    })
  }

  private def inferFunc(expr: N.Expr, func: N.Expr.Func): T.Expr = {
    var inferringPositional = true
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
        val checkedRetTypAnnotation = inferAnnotation(t)
        val expectedRetTyp = annotationToType(checkedRetTypAnnotation)
        (checkExpr(func.body, expectedRetTyp), Some(checkedRetTypAnnotation))
      case None =>
        (inferExpr(func.body), None)
    }
    val from = inferredParams.map(param => {
      (getPatternLabel(param.pattern), param.pattern.typ)
    }).toVector
    T.Expr(
      meta = expr.meta.typed,
      typ = Func(from, checkedBody.typ),
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
          val inferredAnnotation = inferAnnotation(annotation)
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
        val inferredExpr = inferExpr(expr)
        val errors = assertSubType(expr.loc)(inferredExpr.typ, typ)
        inferredExpr.copy(
          meta = inferredExpr.meta.withDiagnostics(errors)
        )
      case (func@N.Expr.Func(_, _, _, _, _, _), _) =>
        checkFunc(expr, func, typ)
    }
  }

  private def checkFunc(expr: N.Expr, func: Named.Expr.Func,  typ: Type): T.Expr = {
    typ match {
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


  private def assertSubType(loc: Loc)(a: Type, b: Type): Iterable[Diagnostic] = {
    val errors = ListBuffer.empty[Diagnostic]
    (a, b) match {
      case (Primitive.Int, Primitive.Int) |
           (Primitive.Bool, Primitive.Bool) |
           (Primitive.Unit, Primitive.Unit)
      =>
        ()
      case (_, Var(v)) =>
        ctx.getTypeVar(v) match {
          case Some(t) =>
            assertSubType(loc)(a, t)
          case None =>
            errors.append(
              Diagnostic(
                TypeMismatch(a, b),
                Severity.Error,
                loc
              )
            )
        }
      case (_, _) =>
        errors.append(
          Diagnostic(
            TypeMismatch(a, b),
            Severity.Error,
            loc
          )
        )
    }
    errors
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
    val resolvedType = resolveType(typ)
    ctx.setTypeOfSymbol(ident.name, resolvedType)
    T.Ident(
      ident.meta.typed,
      ident.name
    )
  }

  private def resolveType(typ: Type): Type = {
    typ match {
      case Var(name) =>
        ctx.getTypeVar(name) match {
          case Some(t) => resolveType(t)
          case None => typ
        }
      case _ => typ
    }
  }

  private def inferAnnotation(annotation: N.TypeAnnotation): T.TypeAnnotation = {
    val variant = annotation.variant match {
      case N.TypeAnnotation.Var(ident) =>
        T.TypeAnnotation.Var(inferIdent(ident))
      case N.TypeAnnotation.Forall(scope, params, innerAnnotation) =>
        val inferredInner = inferAnnotation(innerAnnotation)
        val inferredParams = params.map(param => {
          T.GenericParam(
            meta = param.meta.typed,
            kindAnnotation = param.kindAnnotation.map(inferKindAnnotation),
            ident = inferIdent(param.ident)
          )
        })
        T.TypeAnnotation.Forall(scope, inferredParams, inferredInner)
      case N.TypeAnnotation.Func(params, retType) =>
        val inferredParams = params.map(param => {
          (param._1.map(inferIdent), inferAnnotation(param._2))
        })
        T.TypeAnnotation.Func(
          inferredParams,
          inferAnnotation(retType)
        )
      case N.TypeAnnotation.Error =>
        T.TypeAnnotation.Error
    }
    T.TypeAnnotation(
      meta = annotation.meta.typed,
      variant
    )
  }

  private def inferKindAnnotation(annot: Named.KindAnnotation): T.KindAnnotation = {
    val variant = annot.variant match {
      case N.KindAnnotation.Star => T.KindAnnotation.Star
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
    println(s"checkAnnotation(a")
    val inferredAnnotation = inferAnnotation(annotation)
    val annotTyp = annotationToType(inferredAnnotation)
    val errors = assertSubType(annotation.loc)(annotTyp, typ)
    inferredAnnotation.copy(
      meta = inferredAnnotation.meta.withDiagnostics(errors)
    )
  }

  private def annotationToType(annotation: T.TypeAnnotation): Type = {
    annotation.variant match {
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
        Forall(typParams, annotationToType(inner))
      case T.TypeAnnotation.Error =>
        ErrorType
    }
  }
}
