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

    override def toString: String = ""
  }

  trait HasMeta {
    def getMeta: Meta
  }

  trait HasVariant[T] {
    val variant: T
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
  ) extends NodeOps(meta) with Node with HasVariant[Pattern.Variant] {
    override def children: Iterable[Node] = variant.children
    override def toString: String =
      variant match {
        case Pattern.Var(ident) =>
          ident.name.toString
        case Pattern.Annotated(p, t) =>
          s"""(${p.toString}: ${t.toString})"""
        case Pattern.Error => "<ERROR>"
      }
  }

  case class TypeAnnotation(
    meta: Meta,
    variant: TypeAnnotation.Variant
  ) extends NodeOps(meta) with Node with HasVariant[TypeAnnotation.Variant] { self =>
    override def children: Iterable[Node] = variant.children

    override def toString: String =
      s"""$variant"""
  }

  object TypeAnnotation {
    type T = Variant
    case class Var(ident: Ident) extends T {
      override def toString: String = ident.name.toString
    }
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
        case Func(_, params, returnTypeAnnotation, body) =>
          val patterns = params.map(_.pattern)
          val ret = returnTypeAnnotation
              .map(x => List(x)).getOrElse(List())
          List(patterns, ret, List(body)).flatten
        case Error() => List()
      }
    }
    case class Param(
      pattern: Pattern
    ) {
      override def toString: String = pattern.toString
    }
    type T = Variant
    case class Var(ident: Ident) extends T {
      override def toString: String = s"""${ident.name}"""
    }
    case class Literal(variant: LiteralVariant) extends T
    case class Func(
      scope: _Scope,
      params: Iterable[Param],
      returnTypeAnnotation: Option[TypeAnnotation],
      body: Expr
    ) extends T {
      override def toString: String =
        s"""fn $params:$returnTypeAnnotation => $body"""
    }
    case class Error() extends T
  }

  case class Expr(
    meta: Meta,
    typ: _Type,
    variant: Expr.Variant
  ) extends NodeOps(meta) with Node with HasVariant[Expr.Variant] {
    override def children: Iterable[Node] = variant.children

    override def toString: String = variant.toString
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
  ) extends NodeOps(meta) with Node with HasVariant[Declaration.Variant] {
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