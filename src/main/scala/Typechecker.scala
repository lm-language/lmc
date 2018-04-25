import Syntax._

import scala.collection.mutable
import Diagnostics._


final class Typechecker(compiler: Compiler, setTypeOfSymbol: (Symbol, Type) => Unit) {
  import Syntax.{ Named => N }
  import Syntax.{ Typed => T }

  def checkSourceFile(parsed: Parsed.SourceFile): Typed.SourceFile = {
    val named = new Renamer().renameSourceFile(parsed)
    val declarations = checkModule(parsed.loc, named.declarations)
    Typed.SourceFile(
      meta = named.meta.typed,
      declarations = declarations,
      scope = named.scope
    )
  }

  /**
    * Type checking a module involves the following steps
    * 1. Push a new scope for the module
    * 2. Walk over all declarations, adding all bindings
    *    declared in the module, with the type UnBound
    * 3. Walk over declarations again, this time inferring
    *    the actual types of bindings, assigning UnInferred
    *    for declarations that can't be inferred yet. As we
    *    go along, we keep adding type constraints.
    * 4. Solve all type constraints generated. Ensure that
    *    no un-inferred type remains after solving the constraints.
    *    If there are un-inferred types, add errors to their corresponding
    *    declaration nodes.
    * @param loc Loc of module/source file
    * @param declarations List of declarations
    * @return
    */
  private def checkModule(loc: Loc, declarations: Iterable[N.Declaration]): Iterable[T.Declaration] = {
    declarations.map(checkDeclaration)
  }

  private def checkDeclaration(declaration: N.Declaration): T.Declaration = {
    val variant = declaration.variant match {
      case N.Declaration.Let(binder, expr) =>
        val inferredExpr = inferExpr(expr)
        val boundBinder = bindTypeToBinder(binder, inferredExpr.typ)
        T.Declaration.Let(binder = boundBinder, rhs = inferredExpr)
      case N.Declaration.Error() =>
        T.Declaration.Error()
    }
    T.Declaration(meta = declaration.meta.typed, variant = variant)
  }

  private def inferExpr(expr: N.Expr): T.Expr = {
    import Typed.{Expr => TE}
    import Named.{Expr => NE}
    val (variant: T.Expr.Variant, typ, diagnostics) = expr.variant match {
      case NE.Literal(NE.LInt(l)) =>
        (TE.Literal(TE.LInt(l)), compiler.IntType, List())
      case NE.Var(ident) =>
        val (typ, diagnostics) = getType(ident.loc, ident.name)
        (expr.variant, typ, diagnostics)
      case NE.Error() => (expr.variant, ErrorType(), List())
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

  def getType(loc: Loc, symbol: Symbol): (Type, Iterable[Diagnostic]) = {
    compiler.getType(symbol) match {
      case None =>
        (ErrorType(), List(
          Diagnostic(
            variant = UnBoundVar(symbol.text),
            severity = Severity.Error,
            loc = loc)))
      case Some(t) =>
        (t, List())
    }
  }

  private def bindTypeToBinder(binder: N.Binder, typ: Type): T.Binder = {
    T.Binder(
      meta = binder.meta.typed,
      pattern = bindTypeToPattern(binder.pattern, typ)
    )
  }

  private def bindTypeToPattern(pattern: N.Pattern, typ: Type): T.Pattern = {
    val (variant: T.Pattern.Variant, diagnostics) = pattern.variant match {
      case N.Pattern.Var(ident) =>
        val newIdent = bindTypeToIdent(ident, typ)
        (T.Pattern.Var(newIdent), List())
      case N.Pattern.Error() =>
        (T.Pattern.Error, List())
    }
    T.Pattern(
      meta =
        pattern.meta.copy(
          diagnostics = pattern.meta.diagnostics ++ diagnostics
        ).typed,
      typ = ???,
      variant = variant
    )
  }

  private def bindTypeToIdent(ident: N.Ident, typ: Type): T.Ident = {
    println("Setting type of binding ", ident.name)
    setTypeOfSymbol(ident.name, typ)
    T.Ident(ident.meta.typed, ident.name)
  }
}