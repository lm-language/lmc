package lmc

import lmc.common.ScopeBuilder
import lmc.syntax.Parsed._
import lmc.diagnostics._

/**
  * Adds String -> Symbol mappings in scopes
  * of nodes.
  */
class Renamer(
  ctx: Context.Binder,
  error: Diagnostic => Unit
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
        bindInScope(e.scope, e.ident.name, e, e.ident.loc)
        findDeclParent(e).foreach(d =>
          e.cases.foreach(c => {
            bindIdentifier(d, c.ident)
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
        bindInScope(ident.scope, ident.name,  decl, ident.loc)
    }
  }


  private def bindInScope(scope: ScopeBuilder, name: String, decl: Declaration, loc: common.Loc): Unit = {
    val symbol = ctx.makeSymbol(name, decl)
    scope.getSymbol(name) match {
      case Some(_) =>
        error(
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
