package lmc

import scala.collection.mutable
import lmc.syntax._
import lmc.common.{Loc, ScopeEntry, Symbol}
import lmc.diagnostics.{Diagnostic, Severity, TypeMismatch}
import lmc.types.Kind.Star
import lmc.types._

final class TypeChecker(
  compiler: Compiler,
  setTypeOfSymbol: (Symbol, Type) => Unit,
  setKindOfSymbol: (Symbol, Kind) => Unit,
  makeUninferred: (() => Type)
) {
  import lmc.syntax.{Named => N, Typed => T}

  private val types = {
    val t = mutable.HashMap.empty[Symbol, Type]
    for ((_, ScopeEntry(_, symbol, typ)) <- compiler.PrimitivesScope.symbols) {
      t.put(symbol, typ)
    }
    t
  }

  private val typeVars = {
    val t = mutable.HashMap.empty[Symbol, Type]
    for ((_, (sym, typ, _)) <- compiler.PrimitivesScope.typeSymbols) {
      t.put(sym, typ)
    }
    t
  }

  def checkSourceFile(parsed: Parsed.SourceFile): T.SourceFile = {
    val named = new Renamer(
      (text) => compiler.makeSymbol(text),
      () => this.makeUninferred(),
      compiler.PrimitivesScope
    ).renameSourceFile(parsed)
    val declarations = checkModule(parsed.loc, named.declarations)
    T.SourceFile(
      meta = named.meta.typed,
      declarations = declarations,
      scope = named.scope
    )
  }

  private def checkModule(loc: Loc, declarations: Iterable[N.Declaration]): Iterable[T.Declaration] = {
    declarations.map(checkDeclaration)
  }

  private def checkDeclaration(declaration: N.Declaration): T.Declaration = {
    val variant = declaration.variant match {
      case N.Declaration.Let(pattern, expr) =>
        val (checkedPattern, checkedExpr) = bindExprToPattern(pattern, expr)
        T.Declaration.Let(pattern = checkedPattern, rhs = checkedExpr)
      case N.Declaration.Error() =>
        T.Declaration.Error()
    }
    T.Declaration(meta = declaration.meta.typed, variant = variant)
  }

  private def inferExpr(expr: N.Expr): T.Expr = {
    import Named.{Expr => NE}
    import Typed.{Expr => TE}
    val (variant: TE.Variant, typ: lmc.types.Type, diagnostics) = expr.variant match {
      case NE.Literal(NE.LInt(l)) =>
        (TE.Literal(TE.LInt(l)), Primitive.Int, List())
      case NE.Var(ident) =>
        val typ = getSymbolType(ident.name)
        (expr.variant, typ, List.empty)
      case (NE.Func(scope, params, returnTypeAnnotation, body)) =>
        val typedBody = returnTypeAnnotation match {
          case Some(annot) =>
            val annotatedType = annotationToType(annot)
            checkExpr(body, annotatedType)
          case None => inferExpr(body)
        }
        val typedVariant: TE.Variant = TE.Func(scope, List.empty, None, typedBody)
        val typ: Type = lmc.types.Func(from = List(), to = typedBody.typ)
        (
          typedVariant,
          typ,
          List.empty
        )
      case NE.Error() => (expr.variant, ErrorType, List.empty)
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

  private def getSymbolType(symbol: Symbol): Type = {
    val typ = types.getOrElse(symbol, ErrorType)
    typ
  }

  private def bindExprToPattern(pattern: N.Pattern, expr: N.Expr): (T.Pattern, T.Expr) = {
    val (variant: T.Pattern.Variant, checkedExpr: T.Expr,diagnostics) = pattern.variant match {
      case N.Pattern.Var(ident) =>
        val checkedExpr = inferExpr(expr)
        val typ = checkedExpr.typ
        val newIdent = bindTypeToIdent(ident, typ)
        (T.Pattern.Var(newIdent), checkedExpr, List())
      case N.Pattern.Annotated(p, annotation) =>
        val typ = annotationToType(annotation)
        val checkedAnnotation = checkAnnotation(annotation)
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
        setTypeOfSymbol(ident.name, typ)
        (
          T.Pattern.Var(T.Ident(ident.meta.typed, ident.name)),
          List.empty
        )
      case N.Pattern.Annotated(p, annotation) =>
        val checkedAnnotation = checkAnnotation(annotation)
        val annotationType = annotationToType(annotation)
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
    val (variant, diagnostics) = expr.variant match {
      case N.Expr.Literal(N.Expr.LInt(x)) =>
        val exprTyp = Primitive.Int
        val diagnostics = assertTypeMoreGeneralThan(expr.loc)(typ, exprTyp)
        (T.Expr.Literal(T.Expr.LInt(x)), diagnostics)
      case N.Expr.Var(ident) =>
        val varTyp = getSymbolType(ident.name)
        val diagnostics = assertTypeMoreGeneralThan(loc = ident.loc)(typ, varTyp)
        (T.Expr.Var(ident = T.Ident(meta = ident.meta.typed, ident.name)), diagnostics)
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

  private def checkAnnotation(annotation: Named.TypeAnnotation): T.TypeAnnotation = {
    val typedVariant = annotation.variant match {
      case N.TypeAnnotation.Var(ident) =>
        T.TypeAnnotation.Var(
          T.Ident(meta = ident.meta.typed, ident.name)
        )
      case N.TypeAnnotation.Error =>
        T.TypeAnnotation.Error
    }
    T.TypeAnnotation(
      meta = annotation.meta.typed,
      variant = typedVariant
    )
  }

  private def annotationToType(annotation: N.TypeAnnotation): Type = {
    annotation.variant match {
      case N.TypeAnnotation.Var(ident) =>
        typeVars.get(ident.name) match {
          case Some(typ) =>
            typ
          case None =>
            lmc.types.Var(ident.name)
        }
      case N.TypeAnnotation.Error => ErrorType
    }
  }

  private def bindTypeToIdent(ident: N.Ident, typ: Type): T.Ident = {
    types += ident.name -> typ
    setTypeOfSymbol(ident.name, typ)
    T.Ident(ident.meta.typed, ident.name)
  }
}