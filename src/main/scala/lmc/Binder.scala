package lmc

import lmc.common.{Pos, ScopeBuilder, ScopeEntry, Scope, TypeEntry}
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
          val params1 = params.map(bindGenericParam(scope))
          val typ1 = bindAnnotation(typ)
          annotation.copy(variant = A.Forall(scope, params1, typ1))
        })
      case A.Var(_) =>
        annotation
      case A.TApplication(f, args) =>
        annotation.copy(
          variant =
            A.TApplication(
              bindAnnotation(f),
              args.map(bindAnnotation)
            )
        )
      case A.Error =>
        annotation
    }
  }

  private def bindGenericParam(scope: Scope)(param: GenericParam): GenericParam = {
    scope.getSymbol(param.ident.name) match {
      case Some(_) =>
        param.copy(
          meta = param.meta.withDiagnostic(
            Diagnostic(
              loc = param.loc,
              severity = Severity.Error,
              variant = DuplicateBinding(param.ident.name)
            )
          )
        )
      case None =>
        bindTypeVar(param.ident.name)
        param
    }
  }

  private def withScope[T](scope: ScopeBuilder)(f: (() => T)): T = {
    _scopes = scope::_scopes
    val result = f()
    _scopes = _scopes.tail
    result
  }

  private def bindTypeVar(name: String): Unit = {
    val symbol = ctx.makeSymbol(name)
    currentScope.setTypeVar(name, TypeEntry(symbol))
  }

  def bindDeclaration(decl: Declaration): Declaration = {
    import Declaration._
    decl.variant match {
      case Let(pattern, expr) =>
        val newPattern = bindPattern(pattern)
        val boundExpr = bindExpr(expr)
        decl.copy(
          variant = Let(
            pattern = newPattern,
            boundExpr
          )
        )
      case ExternLet(ident, annotation) =>
        val newAnnotation = bindAnnotation(annotation)
        val boundIdent = bindIdent(ident)
        decl.copy(
          meta = decl.meta,
          variant = ExternLet(boundIdent, newAnnotation)
        )
      case Existential(ident, kindAnnotation) =>
        val (boundIdent, error) = bindTypeDeclHelper(ident)
        val result = decl.copy(
          meta = decl.meta,
          variant = Existential(boundIdent, kindAnnotation)
        )
        error match {
          case Some(e) =>
            result.copy(meta = result.meta.withDiagnostic(e))
          case None => result
        }
      case ExternType(ident, kindAnnotation) =>
        val (boundIdent, error) = bindTypeDeclHelper(ident)
        decl.copy(
          meta = decl.meta.withDiagnostics(error.map({ List(_) }).getOrElse(List())),
          variant = ExternType(boundIdent, kindAnnotation)
        )
      case TypeAlias(ident, kindAnnotation, annotation) =>
        val (boundIdent, error) = bindTypeDeclHelper(ident)
        val boundAnnotation = bindAnnotation(annotation)
        val result = decl.copy(
          meta = decl.meta,
          variant = TypeAlias(boundIdent, kindAnnotation, boundAnnotation)
        )
        error match {
          case Some(e) =>
            result.copy(meta = result.meta.withDiagnostic(e))
          case None => result
        }
      case Error() =>
        decl
    }
  }

  private def bindTypeDeclHelper(ident: Ident) = {
    val error = currentScope.typeSymbols.get(ident.name) match {
      case Some(_) =>
        Some(
          Diagnostic(
            loc = ident.loc,
            variant = DuplicateBinding(
              ident.name
            ),
            severity = Severity.Error
          )
        )
      case None => None
    }
    bindTypeVar(ident.name)
    val boundIdent = ident
    (boundIdent, error)

  }

  private def bindExpr(expr: Expr): Expr = {
    val boundVariant = expr.variant match {
      case Expr.Func(tok, scope, genericParams, params, returnTypeAnnotation, body) =>
        withScope(scope)(() => {
          val boundGenericParams = genericParams.map(bindGenericParam(scope))
          val boundParams = params.map(bindFuncParam)
          val boundAnnotation = returnTypeAnnotation.map(bindAnnotation)
          val boundBody = bindExpr(body)
          Expr.Func(tok, scope, boundGenericParams, boundParams, boundAnnotation, boundBody)
        })
      case Expr.Call(tok, func, args) =>
        Expr.Call(
          tok,
          bindExpr(func),
          args.map(bindArg)
        )
      case Expr.Literal(_) => expr.variant
      case Expr.Var(_) => expr.variant
      case Expr.Error() => expr.variant
    }
    expr.copy(variant = boundVariant)
  }

  private def bindArg(arg: Expr.Arg): Expr.Arg = {
    Expr.Arg(
      label = arg.label,
      value = bindExpr(arg.value)
    )
  }

  private def bindFuncParam(param: Expr.Param): Expr.Param = {
    Expr.Param(bindPattern(param.pattern))
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
          ),
          duplicateBinder = true
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
        val boundAnnotation = bindAnnotation(annotation)
        pattern.copy(
          variant = Pattern.Annotated(newPattern, boundAnnotation)
        )
      case Error =>
        pattern
    }
  }

  private def currentScope: ScopeBuilder = {
    _scopes.head
  }
}
