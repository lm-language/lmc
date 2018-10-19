package lmc

import lmc.common.ScopeBuilder
import lmc.syntax.Parsed._
import lmc.diagnostics._
import lmc.common.Symbol

/**
  * Adds String -> Symbol mappings in scopes
  * of nodes.
  */
class Renamer(
  ctx: Context.Binder,
) {
  def bind(node: Node): Unit = {
    bindWorker(node)
    node.children.foreach(bind)
    postBind(node)
  }

  private def postBind(node: Node): Unit = {
  }

  private def bindWorker(node: Node): Unit = {
    node match {
      case m: Declaration.Module =>
        bindIdentifier(m, m.ident)
      case e: Declaration.Enum =>
        bindInScope(e.scope, e.ident.name, e, e.ident.loc, ctx.makeSymbol(e.ident.name, e))
        findDeclParent(e).foreach(d =>
          e.cases.foreach(c => {
            val symbol = ctx.makeSymbol(c.ident.name, e)
            bindInScope(e.enumScope, c.ident.name, d, c.loc, symbol)
            // the case name should also be bound in the scope outside
            // the enum body
            bindInScope(e.scope, c.ident.name, e, c.loc, symbol)
          }))
      case p: Pattern.Var =>
        val decl = findDeclParent(p)
        decl.foreach(parent =>
          bindIdentifier(parent, p.ident))
      case b: Binder =>
        val decl = findDeclParent(b)
        decl.foreach({ bindIdentifier(_, b.name) })
      case a: Declaration.TypeAlias =>
        bindTypeAlias(a)
      case _ => ()
    }
  }

  private def bindIdentifier(decl: Declaration, ident: Ident): Unit = {
    ident.meta.scope.get match {
      case Some(scope) =>
        val symbol = ctx.makeSymbol(ident.name, decl)
        bindInScope(ident.scope, ident.name,  decl, ident.loc, symbol)
    }
  }


  private def bindInScope(scope: ScopeBuilder, name: String, decl: Declaration, loc: common.Loc, symbol: Symbol): Unit = {
    scope.getSymbol(name) match {
      case Some(_) =>
        ctx.addError(
          Diagnostic(
            loc = loc,
            severity = Severity.Error,
            variant = DuplicateBinding(name)
          )
        )
      case None => ()
    }
    scope.setSymbol(name, symbol)
  }

  private def bindTypeAlias(alias: Declaration.TypeAlias): Unit = {
    bindIdentifier(alias, alias.ident)
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
