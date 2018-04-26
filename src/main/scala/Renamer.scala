import Syntax.{Named => N, Parsed => P}
import Diagnostics._

class Renamer(
  makeSymbol: ((String) => Symbol),
  makeUninferred: () => Type
) {
  var _scopes = List.empty[ScopeBuilder]
  def renameSourceFile(sourceFile: P.SourceFile): N.SourceFile = {
    withScope(sourceFile.scope)(() => {
      val decls = renameModuleDecls(sourceFile.declarations)
      N.SourceFile(
        meta = sourceFile.meta.named,
        scope = sourceFile.scope,
        declarations = decls
      )
    })
  }

  def renameModuleDecls(declarations: Iterable[P.Declaration]): Iterable[N.Declaration] = {
    declarations.map(renameDecl)
  }

  def withScope[T](scope: ScopeBuilder)(f: () => T): T = {
    this._openScope(scope)
    val result = f()
    this._closeScope()
    result
  }

  def renameDecl(decl: P.Declaration): N.Declaration = {
    val (variant, diagnostics) = decl.variant match {
      case P.Declaration.Let(binder, expr) =>
        val namedBinder = renameBinder(binder)
        val namedExpr = renameExpr(expr)
        (N.Declaration.Let(namedBinder, namedExpr), List.empty)
      case P.Declaration.Error() =>
        (N.Declaration.Error(), List.empty)
    }
    N.Declaration(
      meta = decl.meta.copy(
        diagnostics = decl.meta.diagnostics ++ diagnostics
      ).named,
      variant = variant
    )
  }

  def renameBinder(binder: P.Binder): N.Binder = {
    val pattern = renamePattern(binder.pattern)
    N.Binder(binder.meta.named, pattern)
  }

  def renamePattern(pattern: P.Pattern): N.Pattern = {
    val (namedVariant, diagnostics: Iterable[Diagnostic]) = pattern.variant match {
      case P.Pattern.Var(ident) =>
        currentScope.getSymbol(ident.name) match {
          case Some(symbol) =>
            (N.Pattern.Var(makeNamedIdent(ident, symbol)), List(
              Diagnostic(
                loc = pattern.loc,
                severity = Severity.Error,
                variant =  DuplicateBinding(ident.name)
              )
            ))
          case None =>
            val symbol = makeSymbol(ident.name)
            val typ = makeUninferred()
            currentScope.setSymbol(ident.name, ScopeEntry(
              loc = pattern.loc,
              typ = typ,
              symbol = symbol)
            )
            (N.Pattern.Var(makeNamedIdent(ident, symbol)), List.empty[Diagnostic])

        }
      case P.Pattern.Error() =>
        (N.Pattern.Error(), List.empty[Diagnostic])
    }
    val parsedDiagnostics = pattern.meta.diagnostics
    val namedDiagnostics = parsedDiagnostics ++ diagnostics
    N.Pattern(
      meta = pattern.meta.copy(
        diagnostics = namedDiagnostics
      ).named,
      typ = (),
      variant = namedVariant
    )
  }

  private def makeNamedIdent(ident: P.Ident, symbol: Symbol): N.Ident = {
    N.Ident(meta = ident.meta.named, name = symbol)
  }

  def renameExpr(expr: P.Expr): N.Expr = {
    val (namedVariant: N.Expr.Variant, diagnostics) = expr.variant match {
      case P.Expr.Var(ident) =>
          currentScope.getSymbol(ident.name) match {
            case Some(symbol) =>
              (N.Expr.Var(makeNamedIdent(ident, symbol)), List.empty[Diagnostic])
            case None =>
              val symbol = makeSymbol(ident.name)
              val namedIdent = makeNamedIdent(ident, symbol)
              (N.Expr.Var(namedIdent), List(
                Diagnostic(
                  loc = expr.loc,
                  severity = Severity.Error,
                  variant = UnBoundVar(ident.name)
                )
              ))
          }
      case P.Expr.Literal(literalVariant) =>
        (N.Expr.Literal(literalVariant.asInstanceOf[N.Expr.LiteralVariant]), List.empty)
      case P.Expr.Error() =>
        (N.Expr.Error(), List.empty[Diagnostic])
    }
    N.Expr(
      meta = expr.meta.copy(
        diagnostics = expr.meta.diagnostics ++ diagnostics
      ).named,
      typ = (),
      variant = namedVariant
    )
  }

  private def _openScope(scope: ScopeBuilder): Unit = {
    _scopes = scope::_scopes
  }

  private def _closeScope(): Unit = {
    _scopes = _scopes.tail
  }

  def currentScope: ScopeBuilder =
    _scopes.head
}
