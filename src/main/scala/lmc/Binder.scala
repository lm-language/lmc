package lmc

import lmc.common.{Pos, ScopeBuilder, ScopeEntry, Scope, TypeEntry}
import lmc.syntax.Parsed._
import lmc.diagnostics._
import scala.collection.mutable.ListBuffer

/**
  * Adds String -> Symbol mappings in scopes
  * of nodes.
  */
class Binder(
  ctx: Context.Binder
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
        findDeclParent(p).foreach(parent =>
          bindIdentifier(parent, p.ident))
      case a: Declaration.TypeAlias =>
        bindTypeAlias(a)
      case _ => ()
    }
  }

  private def bindIdentifier(decl: Declaration, ident: Ident): Unit = {
    ident.meta.scope.get match {
      case Some(s) =>
        val symbol = ctx.makeSymbol(ident.name)
        s.setSymbol(ident.name, ScopeEntry(symbol))
        ctx.setDeclOf(symbol, decl)
        s.addDeclaration(symbol, decl.meta.id)
    }
  }

  private def bindTypeAlias(alias: Declaration.TypeAlias): Unit = {
    val symbol = ctx.makeSymbol(alias.ident.name)
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
