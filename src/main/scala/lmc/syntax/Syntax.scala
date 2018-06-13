package lmc.syntax

import lmc.common._
import lmc.diagnostics.Diagnostic
import lmc.syntax.token.Token

import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference

trait Syntax {
  type Name
  type _Scope <: Scope

  trait MetaT {
    def id: Int
    def loc: Loc
    def scope: WeakReference[_Scope]
    def parentId: Option[Int]
    def diagnostics: Array[Diagnostic]
    def withDiagnostic(diagnostic: Diagnostic): Meta
    def withDiagnostics(diagnostics: Iterable[Diagnostic]): Meta
  }

  type NodeFlags = Int
  case class Meta(
    id: Int,
    loc: Loc,
    scope: WeakReference[_Scope],
    parentId: Option[Int],
    diagnostics: Array[Diagnostic] = Array()
  ) extends MetaT {
    def typed: lmc.syntax.Typed.Meta = this.asInstanceOf[Typed.Meta]
    def named: Named.Meta = this.asInstanceOf[Named.Meta]

    override def withDiagnostic(diagnostic: Diagnostic): Meta = {
      this.copy(diagnostics = this.diagnostics ++ List(diagnostic))
    }

    override def withDiagnostics(diagnostics: Iterable[Diagnostic]): Meta = {
      this.copy(diagnostics = this.diagnostics ++ diagnostics)
    }

    override def toString: String = ""
  }

  case class MetaBuilder(
    private val _id: Int,
    private var _loc: Loc,
    private val _scope: WeakReference[_Scope],
    private var _parentId: Option[Int],
    private var _diagnostics: Array[Diagnostic],
  ) extends MetaT {
    override val id = _id
    override def loc = _loc
    override def scope = _scope
    override def parentId: Option[Int] = _parentId
    override def diagnostics: Array[Diagnostic] = _diagnostics

    override def withDiagnostic(diagnostic: Diagnostic): Meta =
      Meta(
        _id, _loc, _scope, _parentId,
        _diagnostics :+ diagnostic
      )

    override def withDiagnostics(diagnostics: Iterable[Diagnostic]): Meta =
      Meta(
        _id, _loc, _scope, _parentId,
        _diagnostics ++ diagnostics
      )

    def setDiagnostics(_diagnostics: ListBuffer[Diagnostic]): Unit = {
      this._diagnostics = _diagnostics.toArray
    }

    def setLoc(_loc: Loc): Unit = {
      this._loc = _loc
    }
  }




  sealed trait Node extends HasLoc {
    def meta: Meta

    override def loc = meta.loc
    def errors: Iterable[Diagnostic] = {
      this.meta.diagnostics ++ this.children.flatMap(_.errors)
    }

    def children: Array[Node]
    def scope: Scope = {
      this.meta.scope.get match {
        case None => Scope.empty
        case Some(scope) => scope
      }
    }
  }

  case class Ident(override val meta: Meta, name: Name) extends Node {
    val children = Array()
  }

  sealed trait Declaration extends Node with Expression.Block.Member {
    def modifiers: Set[Declaration.Modifier]
    override def children: Array[Node] =
      this match {
        case Declaration.Let(_, _, ident, Some(rhs)) =>
          Array(ident, rhs)
        case Declaration.Let(_, _, ident, None) =>
          Array(ident)
      }
  }

  case class GenericParam(
    meta: Meta,
    ident: Ident,
    kindAnnotation: Option[KindAnnotation]
  ) extends Node {
    override def children: Array[Node] =
      this match {
        case GenericParam(_, i, Some(annotation)) =>
          Array(i, annotation)
        case GenericParam(_, i, None) =>
          Array(i)
      }
  }
  object Declaration {
    case class Let(
      override val meta: Meta,
      override val modifiers: Set[Declaration.Modifier],
      pattern: Pattern, rhs: Option[Expression]
    ) extends Declaration

    case class TypeAlias(
      override val meta: Meta,
      override val modifiers: Set[Declaration.Modifier],
      ident: Ident,
      kindAnnotation: Option[KindAnnotation],
      rhs: Option[TypeAnnotation]
    ) extends Declaration

    case class Include(
      override val meta: Meta,
      override val modifiers: Set[Declaration.Modifier],
      expr: Expression
    ) extends Declaration

    case class Enum(
      override val meta: Meta,
      override val modifiers: Set[Declaration.Modifier],
      enumScope: _Scope,
      ident: Ident,
      genericParams: Array[GenericParam],
      cases: Array[Enum.Case]
    ) extends Declaration

    case class Error(
      override val meta: Meta,
      override val modifiers: Set[Declaration.Modifier]
    ) extends Declaration

    sealed trait Modifier
    object Modifier {
      case object Extern extends Modifier
      case object Abstract extends Modifier
      case object Override extends Modifier
    }

    object Enum {
      case class Case(
        override val meta: Meta,
        ident: Ident,
        params: Array[(Option[Ident], TypeAnnotation)]
      ) extends Node {
        override def children: Array[Node] = {
          val paramPatterns: Array[Node] = params.flatMap({
            case (Some(ident), annot) => Array[Node](ident, annot)
            case (None, annot) => Array[Node](ident)
          })
          val i: Node = ident
          Array(Array(i), paramPatterns).flatten
        }
      }
    }
  }

  sealed trait Pattern extends Node {
    import Pattern._
    override def children: Array[Node] =
      this match {
        case Var(_, ident) => Array(ident)
        case Annotated(_, inner, annotation) =>
          Array(inner, annotation)
        case DotName(_, ident) => Array(ident)
      }

    def withErrors(errors: Iterable[Diagnostic]): Pattern = {
      this match {
        case Var(meta, ident) => Var(meta.withDiagnostics(errors), ident)
        case Annotated(meta, inner, annotation) =>
          Annotated(meta.withDiagnostics(errors), inner, annotation)
        case DotName(meta, ident) =>
          DotName(meta.withDiagnostics(errors), ident)
      }
    }
  }
  object Pattern {
    case class Var(
      override val meta: Meta,
      ident: Ident
    ) extends Pattern
    case class Annotated(
      override val meta: Meta,
      innerPattern: Pattern,
      typeAnnotation: TypeAnnotation
    ) extends Pattern
    case class DotName(
      override val meta: Meta,
      ident: Ident
    ) extends Pattern
    case class Error(
      override val meta: Meta
    ) extends Pattern
  }

  sealed trait TypeAnnotation extends Node {
    import TypeAnnotation._
    override def children: Array[Node] =
      this match {
        case Var(_, ident) => Array(ident)
      }
  }
  object TypeAnnotation {
    case class Var(meta: Meta, ident: Ident) extends TypeAnnotation
  }


  sealed trait Expression extends Node with Expression.Block.Member {
    import Expression._
    override def children: Array[Node] =
      this match {
        case Literal(_, _) => Array.empty
        case Var(_, i) => Array(i)
      }
  }
  object Expression {
    case class Literal(override val meta: Meta, variant: Literal.Variant) extends Expression
    case class Var(override val meta: Meta, ident: Ident) extends Expression
    case class Func(
      override val meta: Meta,
      fnToken: Token,
      funcScope: _Scope,
      genericParams: Array[GenericParam],
      params: Array[Param],
      returnType: Option[TypeAnnotation],
      body: Expression
    ) extends Expression
    case class Block(
      override val meta: Meta,
      blockScope: _Scope,
      members: Array[Block.Member]
    ) extends Expression

    object Literal {
      sealed trait Variant
      case object Int extends Variant
      case object Float extends Variant
      case object String extends Variant
    }

    object Block {
      sealed trait Member
    }

    case class Param(
      pattern: Pattern
    )
  }

  sealed trait KindAnnotation extends Node {
    import KindAnnotation._
    override def children: Array[Node] = this match {
      case Star(_) => Array.empty
    }
  }
  object KindAnnotation {
    case class Star(override val meta: Meta) extends KindAnnotation
    case class Func(
      override val meta: Meta,
      from: Array[KindAnnotation],
      to: KindAnnotation
    ) extends KindAnnotation
    case class Error(override val meta: Meta) extends KindAnnotation
  }


  case class SourceFile(
    override val meta: Meta,
    override val scope: _Scope,
    declarations: Array[Declaration]
  ) extends Node {
    override def children: Array[Node] = declarations.toArray
  }
}
