package lmc.syntax

import lmc.common._
import lmc.diagnostics.Diagnostic

import scala.ref.WeakReference

trait Syntax {
  type Name
  type _Scope <: Scope
  type _Type

  case class Meta(
    loc: Loc,
    scope: WeakReference[_Scope],
    diagnostics: Iterable[Diagnostic] = List()
  ) extends HasLoc {
    def typed: lmc.syntax.Typed.Meta = this.asInstanceOf[Typed.Meta]
    def named: Named.Meta = this.asInstanceOf[Named.Meta]
  }

  sealed trait HasMeta {
    def getMeta: Meta
  }

  trait HasChildren {
    def children: Iterable[Node]
  }

  sealed trait Node extends HasLoc with HasMeta with HasChildren {
    def errors: Iterable[Diagnostic] = {
      this.getMeta.diagnostics ++ this.children.flatMap(_.errors)
    }
    def getScope: Scope = {
      this.getMeta.scope.get match {
        case None => Scope.empty
        case Some(scope) => scope
      }
    }
  }

  implicit class NodeOps(m: Meta) extends HasMeta with HasLoc {
    def getMeta: Meta = m
    def loc: Loc = m.loc
  }

  case class Ident(meta: Meta, name: Name) extends NodeOps(meta) with Node {
    override def children: Iterable[Node] = List()
  }


  object Pattern {

    type T = Variant
    case class Var(ident: Ident) extends T
    case class Annotated(pattern: Pattern, annotation: TypeAnnotation) extends T
    case object Error extends T

    sealed trait Variant extends HasChildren {
      override def children: Iterable[Node] = this match {
        case Var(ident) => List(ident)
        case Annotated(pattern, annotation) =>
            List(pattern, annotation)
        case Error => List()
      }
    }
  }

  case class Pattern(
    meta: Meta,
    typ: _Type,
    variant: Pattern.Variant
  ) extends NodeOps(meta) with Node {
    override def children: Iterable[Node] = variant.children
  }

  case class TypeAnnotation(
    meta: Meta,
    variant: TypeAnnotation.Variant
  ) extends NodeOps(meta) with Node {
    override def children: Iterable[Node] = variant.children
  }

  object TypeAnnotation {
    type T = Variant
    case class Var(ident: Ident) extends T
    case object Error extends T
    sealed trait Variant extends HasChildren {
      override def children: Iterable[Node] =
        this match {
          case Var(ident) => List(ident)
          case Error => List.empty
        }
    }
  }


  object Expr {
    sealed trait LiteralVariant
    case class LInt(value: Int) extends LiteralVariant
    sealed trait Variant extends HasChildren {
      override def children: Iterable[Node] = this match {
        case Var(ident) => List(ident)
        case Literal(_) => List()
        case Error() => List()
      }
    }
    type T = Variant
    case class Var(ident: Ident) extends T
    case class Literal(variant: LiteralVariant) extends T
    case class Error() extends T
  }

  case class Expr(
    meta: Meta,
    typ: _Type,
    variant: Expr.Variant
  ) extends NodeOps(meta) with Node {
    override def children: Iterable[Node] = variant.children
  }

  object Declaration {
    sealed trait Variant extends HasChildren {
      override def children: Iterable[Node] = this match {
        case Let(pattern, rhs) => List(pattern, rhs)
        case Error() => List()
      }
    }
    type T = Variant
    case class Let(pattern: Pattern, rhs: Expr) extends T
    case class Error() extends T

  }
  case class Declaration(
    meta: Meta,
    variant: Declaration.Variant
  ) extends NodeOps(meta) with Node {
    override def children: Iterable[Node] = variant.children
  }

  case class SourceFile(
    meta: Meta,
    declarations: Iterable[Declaration],
    scope: _Scope
  ) extends NodeOps(meta) with Node {
    override def children: Iterable[Node] = declarations
  }
}
