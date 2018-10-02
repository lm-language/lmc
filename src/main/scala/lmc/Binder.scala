package lmc

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
    postBind(node)
  }

  private def postBind(node: Node): Unit = {
    node match {
      case Declaration.Let(_, pattern, Some(rhs)) =>
      case m: Declaration.Module =>
        m.scope.getSymbol(m.ident.name) match {
          case Some(symbol) =>
            for ((name, s) <- m.moduleScope.symbols) {
              symbol.members.update(name, s)
            }
        }
      case _ => ()
    }
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
        scope.setSymbol(ident.name, symbol)
        ctx.setDeclOf(symbol, decl)
    }
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
