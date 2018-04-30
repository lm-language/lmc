package lmc

import lmc.syntax.{Named => N, Parsed => P}
import lmc.common.{Scope, ScopeBuilder, ScopeEntry}
import lmc.types._
import lmc.diagnostics._

class Renamer(
  makeSymbol: ((String) => common.Symbol),
  makeUninferred: () => Type,
  primitivesScope: Scope
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
      case P.Declaration.Extern(ident, annotation) =>
        val symbol = scopeBuilder.getSymbol(ident.name)
        val namedIdent = makeNamedIdent(ident, symbol.get)
        val namedExpr = renameAnnotation(annotation)
        (N.Declaration.Extern(namedIdent, namedExpr), List.empty)
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

  def renamePattern(pattern: P.Pattern, annotation: Option[P.TypeAnnotation] = None): N.Pattern = {
    val (namedVariant, diagnostics: Iterable[Diagnostic]) = pattern.variant match {
      case P.Pattern.Var(ident) =>
        getEntry(ident.name) match {
          case Some(entry@ScopeEntry(_, symbol, UnAssigned)) =>
            scopeBuilder.setSymbol(symbol.text, entry.copy(typ = makeUninferred()))
            (N.Pattern.Var(makeNamedIdent(ident, symbol)), List.empty)
          case Some(e) =>
            (N.Pattern.Var(makeNamedIdent(ident, e.symbol)), List.empty)
          case None =>
            throw new Error("Compiler bug")

        }
      case P.Pattern.Annotated(pat, annotation) =>
        val newPattern = renamePattern(pat, Some(annotation))
        (N.Pattern.Annotated(
          newPattern,
          renameAnnotation(annotation)
        ), List.empty)

      case P.Pattern.Error =>
        (N.Pattern.Error, List.empty[Diagnostic])
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

  private def renameAnnotation(annotation: P.TypeAnnotation): N.TypeAnnotation = {
    val (
      variant: N.TypeAnnotation.Variant,
      diagnostics: Iterable[Diagnostic]
    ) = annotation.variant match {
      case P.TypeAnnotation.Var(ident) =>
        getTypeEntry(ident.name) match {
          case Some((symbol, _, _)) =>
            val namedIdent = makeNamedIdent(ident, symbol)
            (N.TypeAnnotation.Var(namedIdent),  List.empty)
          case None =>
            val symbol = makeSymbol(ident.name)
            (N.TypeAnnotation.Var(makeNamedIdent(ident, symbol)),
              List(
                Diagnostic(
                  loc = ident.loc,
                  severity = Severity.Error,
                  variant = UnBoundTypeVar(ident.name)
                )
              )
            )
        }
      case P.TypeAnnotation.Func(params, returnType) =>
        val namedParams = params.map(renameAnnotationParam)
        val namedReturnType = renameAnnotation(returnType)
        (N.TypeAnnotation.Func(namedParams, namedReturnType), List.empty)
      case P.TypeAnnotation.Error =>
        (N.TypeAnnotation.Error, List.empty)
    }
    N.TypeAnnotation(
      meta = annotation.meta.copy(
        diagnostics = annotation.meta.diagnostics ++ diagnostics
      ).named,
      variant = variant
    )
  }

  private def renameAnnotationParam(param: P.TypeAnnotation.Param): N.TypeAnnotation.Param = {
    val (labelOpt, typ) = param
    val renamedTyp = renameAnnotation(typ)
    val label = labelOpt map ((ident) =>
      makeNamedIdent(ident, makeSymbol(ident.name))
    )
    (label, renamedTyp)
  }

  private def makeNamedIdent(ident: P.Ident, symbol: common.Symbol): N.Ident = {
    N.Ident(meta = ident.meta.named, name = symbol)
  }

  private def renameExpr(expr: P.Expr): N.Expr = {
    val (namedVariant, diagnostics): (N.Expr.Variant, Iterable[Diagnostic]) = expr.variant match {
      case P.Expr.Var(ident) =>
          getEntry(ident.name) match {
            case Some(ScopeEntry(_, symbol, UnAssigned)) =>
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
              (
                N.Expr.Var(makeNamedIdent(ident, symbol)),
                List.empty[Diagnostic]
              )
            case None =>
              val symbol = makeSymbol(ident.name)
              val namedIdent = makeNamedIdent(ident, symbol)
              (
                N.Expr.Var(namedIdent),
                List(
                  Diagnostic(
                    loc = expr.loc,
                    severity = Severity.Error,
                    variant = UnBoundVar(ident.name)
                  )
                )
              )
          }
      case P.Expr.Literal(literalVariant) =>
        (
          N.Expr.Literal(literalVariant.asInstanceOf[N.Expr.LiteralVariant]),
          List.empty
        )
      case P.Expr.Func(tok, scope, params, annotation, body) =>
        val variant = withScope(scope)(() => {
          val namedParams = params.map((param) => {
            val newPattern = addPatternBindingsToScope(makeUninferred)(param.pattern)
            val result = N.Expr.Param(renamePattern(newPattern))
            result
          })
          val namedAnnotation = annotation.map(renameAnnotation)
          val namedBody = renameExpr(body)
          N.Expr.Func(
            tok,
            scope,
            namedParams,
            namedAnnotation,
            namedBody
          )
        })
        (variant, List.empty)
      case P.Expr.Call(loc, func, args) =>
        val namedFunc = renameExpr(func)
        val namedArgs = args.map((arg) => {
          N.Expr.Arg(arg.label, renameExpr(arg.value))
        })
        (N.Expr.Call(loc, namedFunc, namedArgs), List.empty[Diagnostic])
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

  def getEntry(name: String, scopes: List[Scope] = _scopes): Option[ScopeEntry] = {
    scopes match {
      case sc :: tl =>
        sc.getEntry(name) match {
          case Some(n) => Some(n)
          case None =>
            getEntry(name, tl)
        }
      case _ => primitivesScope.getEntry(name)
    }
  }

  def getTypeEntry(name: String, scopes: List[Scope] = _scopes): Option[Scope.TypeEntry] = {
    scopes match {
      case sc :: tl =>
        sc.typeSymbols.get(name) match {
          case Some(n) => Some(n)
          case None =>
            getTypeEntry(name, tl)
        }
      case _ => primitivesScope.typeSymbols.get(name)
    }
  }

  def scopeBuilder: ScopeBuilder =
    _scopes.head


  def addDeclBindingsToScope(decl: P.Declaration): P.Declaration = {
    import P.Declaration._
    decl.variant match {
      case Let(pattern, expr) =>
        val newPattern = addPatternBindingsToScope(() => UnAssigned)(pattern)
        decl.copy(variant = Let(pattern = newPattern, expr))
      case Extern(ident, _) =>
        val errors = addIdentBindingToScope(ident, makeUninferred, ident.loc)
        decl.copy(
          meta = decl.meta.copy(
            diagnostics = decl.meta.diagnostics ++ errors
          )
        )
      case Error() =>
        decl
    }
  }

  def addIdentBindingToScope(ident: P.Ident, getTyp: (() => Type), loc: common.Loc): Iterable[Diagnostic] = {
    val symbol = makeSymbol(ident.name)
    scopeBuilder.getSymbol(ident.name) match {
      case Some(_) =>
        List(Diagnostic(
          loc = loc,
          severity = Severity.Error,
          variant = DuplicateBinding(ident.name)
        ))
      case None =>
        scopeBuilder.setSymbol(ident.name, common.ScopeEntry(
          symbol = symbol,
          loc = ident.loc,
          typ = getTyp()
        ))
        List()

    }
  }

  def addPatternBindingsToScope(getTyp: (() => Type))(pattern: P.Pattern): P.Pattern = {
    import P.Pattern._
    pattern.variant match {
      case P.Pattern.Var(ident) =>
        val diagnostics = addIdentBindingToScope(ident, getTyp, pattern.loc)
        pattern.copy(meta = pattern.meta.copy(
          diagnostics = pattern.meta.diagnostics ++ diagnostics
        ))
      case P.Pattern.Annotated(pat, annotation) =>
        val newPattern = addPatternBindingsToScope(getTyp)(pat)
        pattern.copy(
          variant = P.Pattern.Annotated(newPattern, annotation)
        )
      case Error =>
        pattern
    }
  }
}
