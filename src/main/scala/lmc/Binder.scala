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
    postBind(node)
  }

  private def postBind(node: Node): Unit = {
    node match {
      case Declaration.Let(_, _,  pattern, Some(rhs)) =>
        assignRhsToPattern(pattern, rhs)
      case m: Declaration.Module =>
        m.scope.getSymbol(m.ident.name) match {
          case Some(symbol) =>
            for ((name, ScopeEntry(s, _)) <- m.moduleScope.symbols) {
              symbol.members.update(name, s)
            }
        }
      case _ => ()
    }
  }

  private def assignRhsToPattern(pattern: Pattern, expression: Expression): Unit = {
    expression match {
      case v: Expression.Var =>
        v.ident.scope.getSymbol(v.ident.name) match {
          case Some(associated) =>
            ctx.setAssociatedSymbol(v.meta.id, associated)
            ctx.setAssociatedSymbol(v.ident.meta.id, associated)
          case None =>
            ()
        }
      case _ => ()
    }
    pattern match {
      case Pattern.Var(_, ident) =>
        ctx.getAssociatedSymbol(expression.meta.id) match {
          case Some(associated) =>
            ident.scope.getSymbol(ident.name) match {
              case Some(symbol) =>
                ctx.setAssociatedSymbol(symbol, associated)
            }
          case None =>
            ()
        }
      case Pattern.Annotated(_, inner, _) =>
        assignRhsToPattern(inner, expression)
      case Pattern.Paren(_, inner) =>
        assignRhsToPattern(inner, expression)
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

        scope.setSymbol(ident.name, ScopeEntry(symbol))
        ctx.setDefIdentId(symbol, ident.meta.id)

        ctx.setDeclOf(symbol, decl)
        ctx.initializeTypeOfSymbol(symbol)
        scope.addDeclaration(symbol, decl.meta.id)
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
