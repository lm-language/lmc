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
    declarations
      .map(addDeclBindingsToScope)
      .map(renameDecl)
  }

  def withScope[T](scope: ScopeBuilder)(f: () => T): T = {
    this._openScope(scope)
    val result = f()
    this._closeScope()
    result
  }

  def renameDecl(decl: P.Declaration): N.Declaration = {
    val (variant, diagnostics) = decl.variant match {
      case P.Declaration.Let(pattern, expr) =>
        val namedPattern = renamePattern(pattern)
        val namedExpr = renameExpr(expr)
        (N.Declaration.Let(namedPattern, namedExpr), List.empty)
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

  def renamePattern(pattern: P.Pattern): N.Pattern = {
    val (namedVariant, diagnostics: Iterable[Diagnostic]) = pattern.variant match {
      case P.Pattern.Var(ident) =>
        currentScope.getEntry(ident.name) match {
          case Some(entry@ScopeEntry(_, symbol, UnAssigned())) =>
            currentScope.setSymbol(symbol.text, entry.copy(typ = makeUninferred()))
            (N.Pattern.Var(makeNamedIdent(ident, symbol)), List.empty)
          case Some(e) =>
            (N.Pattern.Var(makeNamedIdent(ident, e.symbol)), List.empty)
          case None =>
            throw new Error("Compiler bug")

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
          currentScope.getEntry(ident.name) match {
            case Some(ScopeEntry(_, symbol, UnAssigned())) =>
              val namedIdent = makeNamedIdent(ident, symbol)
              (N.Expr.Var(namedIdent), List(
                Diagnostic(
                  loc = expr.loc,
                  severity = Severity.Error,
                  variant = UseBeforeAssignment(ident.name)
                )
              ))
            case Some(entry) =>
              val symbol = entry.symbol
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


  def addDeclBindingsToScope(decl: P.Declaration): P.Declaration = {
    import P.Declaration._
    decl.variant match {
      case Let(pattern, expr) =>
        val newPattern = addPatternBindingsToScope(UnAssigned())(pattern)
        decl.copy(variant = Let(pattern = newPattern, expr))
      case Error() =>
        decl
    }
  }

  def addPatternBindingsToScope(typ: Type)(pattern: P.Pattern): P.Pattern = {
    import P.Pattern._
    pattern.variant match {
      case Var(ident) =>
        currentScope.getSymbol(ident.name) match {
          case Some(_) =>
            pattern.copy(meta = pattern.meta.copy(
              diagnostics = pattern.meta.diagnostics ++ List(
                Diagnostic(
                  loc = pattern.loc,
                  severity = Severity.Error,
                  variant = DuplicateBinding(ident.name)
                )
              )
            ))
          case None =>
            val symbol = makeSymbol(ident.name)
            currentScope.setSymbol(ident.name, ScopeEntry(
              symbol = symbol,
              loc = ident.loc,
              typ = typ
            ))
            pattern
        }
      case Error() =>
        pattern
    }
  }
}
