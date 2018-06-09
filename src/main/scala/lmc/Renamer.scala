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
      makeNamedMeta(sourceFile, null),
      sourceFile.declarations.map(renameDeclaration(sourceFile)),
      sourceFile.scope
    )
  }

  def renameDeclaration(parent: P.Node)(decl: P.Declaration): N.Declaration = {
    ctx.setParsedParentOf(decl, parent)
    ctx.getNamedDecl(decl.meta.id) match {
      case Some(d) => d
      case None =>
        val named = renameDeclarationHelper(parent)(decl)
        ctx.setNamedDecl(decl.meta.id, named)
        named
    }
  }
  def renameDeclarationHelper(parent: P.Node)(decl: P.Declaration): N.Declaration = {
    val variant = decl.variant match {
      case P.Declaration.Let(pattern, rhs) =>
        val namedPattern = renamePattern(decl)(pattern)
        val namedExpr = rhs.map(renameExpr(decl))
        N.Declaration.Let(namedPattern, namedExpr)
    }
    N.Declaration(
      makeNamedMeta(decl, parent),
      variant,
      decl.modifiers.map(P.Declaration.Modifier.named)
    )
  }

  def renameExpr(parent: P.Node)(expr: P.Expr): N.Expr = {
    ctx.setParsedParentOf(expr, parent)
    ctx.getNamedExpr(expr.meta.id) match {
      case Some(e) => e
      case None =>
        val named = renameExprHelper(parent)(expr)
        ctx.setNamedExpr(expr.meta.id, named)
        named
    }
  }
  def renameExprHelper(parent: P.Node)(expr: P.Expr): N.Expr = {
    val variant = expr.variant match {
      case P.Expr.Literal(P.Expr.LInt(i)) =>
        N.Expr.Literal(N.Expr.LInt(i))
      case P.Expr.Var(ident) =>
        N.Expr.Var(renameVarIdent(expr)(ident))
      case P.Expr.Error() =>
        N.Expr.Error()
    }
    N.Expr(
      meta = makeNamedMeta(expr, parent),
      typ = (),
      variant
    )
  }


  def renamePattern(parent: P.Node)(pattern: P.Pattern): N.Pattern = {
    ctx.setParsedParentOf(pattern, parent)
    ctx.getNamedPattern(pattern.meta.id) match {
      case Some(p) => p
      case None =>
        val named = renamePatternHelper(parent)(pattern)
        ctx.setNamedPattern(pattern.meta.id, named)
        named
    }
  }
  def renamePatternHelper(parent: P.Node)(pattern: P.Pattern): N.Pattern = {
    val variant = pattern.variant match {
      case P.Pattern.Var(ident) =>
        val namedIdent = renameBindingIdent(pattern)(ident)
        N.Pattern.Var(namedIdent)
      case P.Pattern.Annotated(p, annotation) =>
        N.Pattern.Annotated(
          renamePattern(pattern)(p),
          renameTypeAnnotation(pattern)(annotation)
        )
    }
    N.Pattern(
      makeNamedMeta(pattern, parent),
      typ = (),
      variant
    )
  }

  def renameBindingIdent(parent: P.Node)(ident: P.Ident): N.Ident = {
    ctx.setParsedParentOf(ident, parent)
    ident.getScope.getEntry(ident.name) match {
      case Some(entry) =>
        val symbol = ctx.makeSymbol(ident.name)
        N.Ident(
          makeNamedMeta(ident, parent).withDiagnostic(Diagnostic(
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
          makeNamedMeta(ident, parent),
          name = symbol,
          duplicateBinder = ident.duplicateBinder
        )
    }
  }

  def renameTypeAnnotation(parent: P.Node)(annotation: P.TypeAnnotation): N.TypeAnnotation = {
    ctx.setParsedParentOf(annotation, parent)
    val variant = annotation.variant match {
      case P.TypeAnnotation.Var(ident) =>
        N.TypeAnnotation.Var(renameTVarIdent(parent)(ident))
      case P.TypeAnnotation.Error =>
        N.TypeAnnotation.Error
    }
    N.TypeAnnotation(
      makeNamedMeta(annotation, parent),
      kind = (),
      variant
    )
  }

  def renameTVarIdent(parent: P.Node)(ident: P.Ident): N.Ident = {
    ctx.setParsedParentOf(ident, parent)
    ident.getScope.resolveTypeEntry(ident.name) match {
      case Some(TypeEntry(sym)) =>
        N.Ident(
          makeNamedMeta(ident, parent),
          name = sym,
          duplicateBinder = ident.duplicateBinder
        )
      case None =>
        val sym = ctx.makeSymbol(ident.name)
        N.Ident(
          makeNamedMeta(ident, parent).withDiagnostic(
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
    ctx.setParsedParentOf(ident, parent)
    ident.getScope.resolveEntry(ident.name) match {
      case Some(entry) =>
        N.Ident(
          makeNamedMeta(ident, parent),
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
              renameDeclaration(ident)(parsedDecl)
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
                makeNamedMeta(ident, parent).withDiagnostics(errors),
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
              makeNamedMeta(ident, parent).withDiagnostic(error),
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
    ctx.getParsedParentOf(node.getMeta.id) match {
      case Some(decl@P.Declaration(_, _, _)) => decl
      case Some(parent) =>
        getCurrentDecl(parent)
      case None =>
        throw new Error(s"Compiler bug: Node $node doesn't have a declaration; ${node.loc}")
    }
  }

  private def makeNamedMeta(node: P.Node, parent: P.Node): N.Meta = {
    if (parent == null) {
     node.getMeta.copy(parent = -1).asInstanceOf[N.Meta]
    } else {
      ctx.setParsedParentOf(node, parent)
      node.getMeta.copy(parent = parent.getMeta.id).asInstanceOf[N.Meta]
    }
  }

}
