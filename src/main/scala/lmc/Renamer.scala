package lmc

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
        val symbol = ctx.makeSymbol(ident.name, decl, getDeclTerm(decl))
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
    }
  }

  private def getDeclTerm(declaration: Declaration): Term = {
    declaration match {
      case Declaration.Let(_, _, Some(rhs)) => rhs
      case Declaration.Module(meta, _, moduleScope, body) =>
        Term.Module(meta, moduleScope.asInstanceOf[_Scope], body)
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
