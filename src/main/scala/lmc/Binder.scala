package lmc

import lmc.common.{ScopeBuilder, ScopeEntry, Symbol, Pos}
import lmc.syntax.Parsed._
import lmc.diagnostics._

/**
  * Adds String -> Symbol mappings in scopes
  * of nodes.
  */
class Binder(
  ctx: Context
) {
  private var _scopes: List[ScopeBuilder] = List()

  def bindSourceFile(sourceFile: SourceFile): SourceFile = {
    withScope(sourceFile.scope)(() => {
      val decls = sourceFile.declarations.map(bindDeclaration)
      sourceFile.copy(declarations = decls)
    })
  }

  private def bindAnnotation(annotation: TypeAnnotation): TypeAnnotation = {
    import syntax.Parsed.{ TypeAnnotation => A }
    annotation.variant match {
      case A.Func(from, to) =>
        val from1 = from.map((param) => {
          val (symbol, typ) = param
          val typ1 = bindAnnotation(typ)
          (symbol, typ1)
        })
        val to1 = bindAnnotation(to)
        annotation.copy(variant = TypeAnnotation.Func(from1, to1))
      case A.Forall(scope, params, typ) =>
        withScope(scope)(() => {
          val params1 = params.map((param) => {
            scope.getSymbol(param.ident.name) match {
              case Some(_) =>
                throw new Error(s"Duplicate param ${param.ident.name}")
                param.copy(
                  meta = param.meta.copy()
                )
              case None =>
                bindName(param.ident.name)
                param
            }
          })
          val typ1 = bindAnnotation(typ)
          annotation.copy(variant = A.Forall(scope, params1, typ1))
        })
      case A.Var(_) =>
        annotation
      case A.Error =>
        annotation
    }
  }

  private def withScope[T](scope: ScopeBuilder)(f: (() => T)): T = {
    _scopes = scope::_scopes
    val result = f()
    _scopes = _scopes.tail
    result
  }

  private def bindName(name: String): Symbol = {
    val symbol = ctx.makeSymbol(name)
    currentScope.setSymbol(name, ScopeEntry(symbol))
    symbol
  }

  private def resolveSymbol(name: String): Option[Symbol] = {
    currentScope.resolveSymbol(name) match {
      case Some(s) => Some(s)
      case None => ctx.PrimitiveScope.resolveSymbol(name)
    }
  }

  def bindDeclaration(decl: Declaration): Declaration = {
    import Declaration._
    decl.variant match {
      case Let(pattern, expr) =>
        val newPattern = bindPattern(pattern)
        decl.copy(variant = Let(pattern = newPattern, expr))
      case Extern(ident, annotation) =>
        val newAnnotation = bindAnnotation(annotation)
        decl.copy(
          meta = decl.meta,
          variant = Extern(ident, newAnnotation)
        )
      case Error() =>
        decl
    }
  }

  def bindIdent(ident: Ident, validAfter: Option[Pos] = None): Ident = {
    val symbol = ctx.makeSymbol(ident.name)
    currentScope.getSymbol(ident.name) match {
      case Some(_) =>
        ident.copy(
          meta = ident.meta.withDiagnostic(
            Diagnostic(
              loc = ident.loc,
              severity = Severity.Error,
              variant = DuplicateBinding(ident.name)
            )
          )
        )
      case None =>
        currentScope.setSymbol(
          ident.name,
          ScopeEntry(symbol, validAfter)
        )
        ident

    }
  }

  def bindPattern(pattern: Pattern): Pattern = {
    import Pattern._
    pattern.variant match {
      case Pattern.Var(ident) =>
        val boundIdent = bindIdent(ident, Some(pattern.loc.end))
        pattern.copy(variant = Pattern.Var(boundIdent))
      case Pattern.Annotated(pat, annotation) =>
        val newPattern = bindPattern(pat)
        pattern.copy(
          variant = Pattern.Annotated(newPattern, annotation)
        )
      case Error =>
        pattern
    }
  }

  private def currentScope: ScopeBuilder = {
    _scopes.head
  }
}
