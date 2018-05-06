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
      case N.Expr.Error() =>
        T.Expr(
          meta = expr.meta.typed,
          typ = ErrorType,
          variant = T.Expr.Error()
        )
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
    println("HERE", ident.duplicateBinder)
    (ident.duplicateBinder, ctx.getTypeOfSymbol(ident.name)) match {
      case (false, Some(t)) =>
        throw new Error(
          s"Compiler bug: Tried to assign type to already assigned ident $ident".concat(
            s" (loc: ${ident.loc}, old type: $t; new type: $typ)"
          )
        )
      case _ => ()
    }
    ctx.setTypeOfSymbol(ident.name, typ)
    T.Ident(
      ident.meta.typed,
      ident.name
    )
  }

  private def inferAnnotation(annotation: N.TypeAnnotation): T.TypeAnnotation = {
    val variant = annotation.variant match {
      case N.TypeAnnotation.Var(ident) =>
        T.TypeAnnotation.Var(inferIdent(ident))
      case N.TypeAnnotation.Error =>
        T.TypeAnnotation.Error
    }
    T.TypeAnnotation(
      meta = annotation.meta.typed,
      variant
    )
  }

  private def inferIdent(ident: N.Ident): T.Ident = {
    T.Ident(
      meta = ident.meta.typed,
      name = ident.name
    )
  }

  private def checkAnnotation(annotation: N.TypeAnnotation, typ: Type): T.TypeAnnotation = {
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
      case T.TypeAnnotation.Error =>
        ErrorType
    }
  }
}
