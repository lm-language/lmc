package lmc

import lmc.common.{ScopeEntry, TypeEntry}
import lmc.syntax.Parsed._
import lmc.diagnostics._

/**
  * Adds String -> Symbol mappings in scopes
  * of nodes.
  */
class Binder(
  ctx: Context.Binder,
  error: Diagnostic => Unit
) {
  def bind(node: Node): Unit = {
    bindWorker(node)
    node.children.foreach(bind)
  }

  private def bindWorker(node: Node): Unit = {
    node match {
      case m: Declaration.Module =>
        bindIdentifier(m, m.ident)
      case p: Pattern.Var =>
        val decl = findDeclParent(p)
        decl.foreach(parent =>
          bindIdentifier(parent, p.ident))
      case a: Declaration.TypeAlias =>
        bindTypeAlias(a)
      case _ => ()
    }
  }

  private def bindIdentifier(decl: Declaration, ident: Ident): Unit = {
    ident.meta.scope.get match {
      case Some(scope) =>
        val symbol = ctx.makeSymbol(ident.name)
        scope.getSymbol(ident.name) match {
          case Some(_) =>
            error(
              Diagnostic(
                loc = ident.loc,
                severity = Severity.Error,
                variant = DuplicateBinding(ident.name)
              )
            )
          case None => ()
        }

        scope.setSymbol(ident.name, ScopeEntry(symbol))
        ctx.setDeclOf(symbol, decl)
        scope.addDeclaration(symbol, decl.meta.id)
    }
  }

  private def bindTypeAlias(alias: Declaration.TypeAlias): Unit = {
    val symbol = ctx.makeSymbol(alias.ident.name)
    alias.scope.typeSymbols.get(alias.ident.name) match {
      case Some(_) =>
        error(
          Diagnostic(
            loc = alias.ident.loc,
            severity = Severity.Error,
            variant = DuplicateBinding(alias.ident.name)
          )
        )
      case None => ()
    }
    alias.scope.setTypeVar(alias.ident.name, TypeEntry(symbol))
  }

  private def findDeclParent(node: Node): Option[Declaration] = {
    node match {
      case d: Declaration => Some(d)
      case _ =>
        node.meta.parentId
          .flatMap(ctx.getParsedNode)
          .flatMap(findDeclParent)
    }
  }
}
