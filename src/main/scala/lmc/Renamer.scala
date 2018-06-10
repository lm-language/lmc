package lmc

import lmc.syntax.{Named => N, Parsed => P}
import lmc.common._
import lmc.diagnostics._


class Renamer(
  ctx: Context.Renamer
) {
  def renameSourceFile(sourceFile: P.SourceFile): N.SourceFile = {
    ctx.getNamedSourceFile(sourceFile.meta.id) match {
      case Some(s) => s
      case None =>
        val named = renameSourceFileHelper(sourceFile)
        ctx.setNamedSourceFile(sourceFile.meta.id, named)
        named
    }
  }
  def renameSourceFileHelper(sourceFile: P.SourceFile): N.SourceFile = {
    N.SourceFile(
      makeNamedMeta(sourceFile),
      sourceFile.declarations.map(renameDeclaration),
      sourceFile.scope
    )
  }

  def renameDeclaration(decl: P.Declaration): N.Declaration = {
    ctx.getNamedDecl(decl.meta.id) match {
      case Some(d) => d
      case None =>
        val named = renameDeclarationHelper(decl)
        ctx.setNamedDecl(decl.meta.id, named)
        named
    }
  }

  def renameDeclarationHelper(decl: P.Declaration): N.Declaration = {
    val variant = decl.variant match {
      case P.Declaration.Let(pattern, rhs) =>
        val namedPattern = renamePattern(pattern)
        val namedExpr = rhs.map(renameExpr)
        N.Declaration.Let(namedPattern, namedExpr)
      case P.Declaration.TypeAlias(ident, kindAnnotation, rhs) =>
        val namedIdent = renameBindingTypeIdent(ident)
        val namedKindAnnotation = kindAnnotation.map(renameKindAnnotation)
        val namedRhs = rhs.map(renameTypeAnnotation)
        N.Declaration.TypeAlias(
          namedIdent,
          namedKindAnnotation,
          namedRhs
        )
      case P.Declaration.Error() =>
        N.Declaration.Error()
    }
    N.Declaration(
      makeNamedMeta(decl),
      variant,
      decl.modifiers.map(P.Declaration.Modifier.named)
    )
  }

  def renameKindAnnotation(k: P.KindAnnotation): N.KindAnnotation = {
    var variant = k.variant match {
      case P.KindAnnotation.Star => N.KindAnnotation.Star
      case P.KindAnnotation.KFun(from, to) => N.KindAnnotation.KFun(
        from.map(renameKindAnnotation),
        renameKindAnnotation(to)
      )
      case P.KindAnnotation.Error => N.KindAnnotation.Error
    }
    N.KindAnnotation(
      k.meta.named,
      variant
    )
  }

  def renameExpr(expr: P.Expr): N.Expr = {
    ctx.getNamedExpr(expr.meta.id) match {
      case Some(e) => e
      case None =>
        val named = renameExprHelper(expr)
        ctx.setNamedExpr(expr.meta.id, named)
        named
    }
  }
  def renameExprHelper(expr: P.Expr): N.Expr = {
    val variant = expr.variant match {
      case P.Expr.Literal(P.Expr.LInt(i)) =>
        N.Expr.Literal(N.Expr.LInt(i))
      case P.Expr.Var(ident) =>
        N.Expr.Var(renameVarIdent(expr)(ident))
      case P.Expr.Call(argsLoc, func, args) =>
        val namedFunc = renameExpr(func)
        val namedArgs = args map {
          case P.Expr.Arg(label, value) =>
            val namedValue = renameExpr(value)
            N.Expr.Arg(label, namedValue)
        }
        N.Expr.Call(argsLoc, namedFunc, namedArgs)
      case P.Expr.Error() =>
        N.Expr.Error()
    }
    N.Expr(
      meta = makeNamedMeta(expr),
      typ = (),
      variant
    )
  }


  def renamePattern(pattern: P.Pattern): N.Pattern = {
    ctx.getNamedPattern(pattern.meta.id) match {
      case Some(p) => p
      case None =>
        val named = renamePatternHelper(pattern)
        ctx.setNamedPattern(pattern.meta.id, named)
        named
    }
  }
  def renamePatternHelper(pattern: P.Pattern): N.Pattern = {
    val variant = pattern.variant match {
      case P.Pattern.Var(ident) =>
        val namedIdent = renameBindingIdent(ident)
        N.Pattern.Var(namedIdent)
      case P.Pattern.Annotated(p, annotation) =>
        N.Pattern.Annotated(
          renamePattern(p),
          renameTypeAnnotation(annotation)
        )
    }
    N.Pattern(
      makeNamedMeta(pattern),
      typ = (),
      variant
    )
  }

  def renameBindingTypeIdent(ident: P.Ident): N.Ident = {
    ident.getScope.typeSymbols.get(ident.name) match {
      case Some(entry) =>
        val symbol = ctx.makeSymbol(ident.name)
        N.Ident(
          makeNamedMeta(ident).withDiagnostic(Diagnostic(
            variant = DuplicateBinding(ident.name),
            severity = Severity.Error,
            loc = ident.loc
          )),
          name = symbol,
          duplicateBinder = true
        )
      case None =>
        val symbol = ctx.makeSymbol(ident.name)
        ident.getScope.setTypeVar(ident.name, TypeEntry(symbol))
        N.Ident(
          makeNamedMeta(ident),
          name = symbol,
          duplicateBinder = ident.duplicateBinder
        )
    }
  }

  def renameBindingIdent(ident: P.Ident): N.Ident = {
    ident.getScope.getEntry(ident.name) match {
      case Some(entry) =>
        val symbol = ctx.makeSymbol(ident.name)
        N.Ident(
          makeNamedMeta(ident).withDiagnostic(Diagnostic(
            variant = DuplicateBinding(ident.name),
            severity = Severity.Error,
            loc = ident.loc
          )),
          name = symbol,
          duplicateBinder = true
        )
      case None =>
        val symbol = ctx.makeSymbol(ident.name)
        ident.getScope.setSymbol(ident.name, ScopeEntry(symbol))
        N.Ident(
          makeNamedMeta(ident),
          name = symbol,
          duplicateBinder = ident.duplicateBinder
        )
    }
  }

  def renameTypeAnnotation(annotation: P.TypeAnnotation): N.TypeAnnotation = {
    val variant = annotation.variant match {
      case P.TypeAnnotation.Var(ident) =>
        N.TypeAnnotation.Var(renameTVarIdent(ident))
      case P.TypeAnnotation.Forall(scope, params, inner) =>
        val namedParams = params.map(param => {
          val namedIdent = renameBindingTypeIdent(param.ident)
          val namedKindAnnotation = param.kindAnnotation.map(renameKindAnnotation)
          N.GenericParam(
            param.meta.named,
            param.kind,
            namedIdent,
            namedKindAnnotation
          )
        })
        val namedInner = renameTypeAnnotation(inner)
        N.TypeAnnotation.Forall(scope, namedParams, namedInner)
      case P.TypeAnnotation.TApplication(tf, args) =>
        val namedTf = renameTypeAnnotation(tf)
        val namedArgs = args.map(renameTypeAnnotation)
        N.TypeAnnotation.TApplication(namedTf, namedArgs)
      case P.TypeAnnotation.Func(params, returnType) =>
        val namedParams = params map {
          case (label, paramAnnotation) =>
            val namedLabel = label.map(renameBindingIdent)
            val namedTypeAnnotation = renameTypeAnnotation(paramAnnotation)
            (namedLabel, namedTypeAnnotation)
        }
        val namedReturnType = renameTypeAnnotation(returnType)
        N.TypeAnnotation.Func(
          namedParams, namedReturnType
        )
      case P.TypeAnnotation.Error =>
        N.TypeAnnotation.Error
    }
    N.TypeAnnotation(
      makeNamedMeta(annotation),
      kind = (),
      variant
    )
  }

  def renameTVarIdent(ident: P.Ident): N.Ident = {
    ident.getScope.resolveTypeEntry(ident.name) match {
      case Some(TypeEntry(sym)) =>
        N.Ident(
          makeNamedMeta(ident),
          name = sym,
          duplicateBinder = ident.duplicateBinder
        )
      case None =>
        val sym = ctx.makeSymbol(ident.name)
        N.Ident(
          makeNamedMeta(ident).withDiagnostic(
            Diagnostic(
              loc = ident.meta.loc,
              severity = Severity.Error,
              variant = UnBoundTypeVar(ident.name)
            )
          ),
          name = sym,
          duplicateBinder = ident.duplicateBinder
        )
    }
  }

  def renameVarIdent(parent: P.Node)(ident: P.Ident): N.Ident = {
    ident.getScope.resolveEntry(ident.name) match {
      case Some(entry) =>
        N.Ident(
          makeNamedMeta(ident),
          name = entry.symbol,
          duplicateBinder = ident.duplicateBinder
        )
      case None =>
        val parsedDeclOpt = getDeclOfVar(ident)
        val currentDecl = getCurrentDecl(ident)
        parsedDeclOpt match {
          case Some(parsedDecl) =>
            if (currentDecl.meta.id == parsedDecl.meta.id) {
              throw new Error("Cyclic declaration")
            } else {
              renameDeclaration(parsedDecl)
              val errors = parsedDecl.variant match {
                case P.Declaration.Let(_, _) =>
                  Array(
                    Diagnostic(
                      loc = ident.loc,
                      severity = Severity.Error,
                      variant = UseBeforeAssignment(ident.name)
                    )
                  )
              }
              val entry = ident.getScope.resolveEntry(ident.name).get
              N.Ident(
                makeNamedMeta(ident).withDiagnostics(errors),
                name = entry.symbol,
                duplicateBinder = ident.duplicateBinder
              )
            }
          case None =>
            val error = Diagnostic(
              loc = ident.loc,
              severity = Severity.Error,
              variant = UnBoundVar(ident.name)
            )
            N.Ident(
              makeNamedMeta(ident).withDiagnostic(error),
              name = ctx.makeSymbol(ident.name),
              duplicateBinder = ident.duplicateBinder
            )
        }
    }
  }


  def getDeclOfVar(ident: P.Ident): Option[P.Declaration] = {
    val nodeOpt = ident.meta.scope.get.flatMap(_.parsedNode).flatMap(_.get)
    nodeOpt match {
      case Some(P.SourceFile(_, declarations, _)) =>
        declarations.find(isTermDeclarationFor(ident.name))
      case Some(P.Expr(_, _, P.Expr.Block(_, declarations))) =>
        declarations.map({
          case P.Expr(_, _, _) => null
          case d@P.Declaration(_, _, _) => d
        }).filter(_ != null).find(isTermDeclarationFor(ident.name))
      case _ =>
        None
    }
  }

  def isTermDeclarationFor(name: String)(decl: P.Declaration): Boolean = {
    decl.variant match {
      case P.Declaration.Let(pattern, _) =>
        isBindingPatternFor(name)(pattern)
    }
  }

  def isBindingPatternFor(name: String)(pattern: P.Pattern): Boolean = {
    pattern.variant match {
      case P.Pattern.Var(ident) => ident.name == name
      case P.Pattern.Annotated(inner, _) => isBindingPatternFor(name)(inner)
      case P.Pattern.Error => false
    }
  }

  def getCurrentDecl(node: P.Node): P.Declaration = {
    ctx.getParsedNode(node.getMeta.parent) match {
      case Some(decl@P.Declaration(_, _, _)) => decl
      case Some(parent) =>
        if (parent.getMeta.id == node.getMeta.id) {
          throw new Error(s"Node $node ${node.getMeta.id} points to itself")
        }
        getCurrentDecl(parent)
      case None =>
        throw new Error(s"Compiler bug: Node $node doesn't have a declaration; ${node.loc}")
    }
  }

  private def makeNamedMeta(node: P.Node): N.Meta = {
    node.getMeta.named
  }

}
