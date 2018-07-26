package lmc.syntax

import lmc.common._
import lmc.diagnostics.Diagnostic
import lmc.syntax.token.Token

import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference

trait Syntax {
  type Name
  type _Scope <: Scope
  type _Type

  trait Meta {
    def id: Int
    def loc: Loc
    def scope: WeakReference[_Scope]
    def parentId: Option[Int]
    def diagnostics: Array[Diagnostic]
    def typ: _Type
    def withDiagnostic(diagnostic: Diagnostic): Meta
    def withDiagnostics(diagnostics: Iterable[Diagnostic]): Meta
    def typed(typ: lmc.types.Type): Typed.Meta =
      Typed.ImmutableMeta(
        id, loc, scope, parentId,
        diagnostics, typ
      )
  }

  type NodeFlags = Int
  case class ImmutableMeta(
    id: Int,
    loc: Loc,
    scope: WeakReference[_Scope],
    parentId: Option[Int],
    diagnostics: Array[Diagnostic] = Array(),
    typ: _Type
  ) extends Meta {
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
    typ: _Type
  ) extends Meta {
    override val id = _id
    override def loc = _loc
    override def scope = _scope
    override def parentId: Option[Int] = _parentId
    override def diagnostics: Array[Diagnostic] = _diagnostics

    override def withDiagnostic(diagnostic: Diagnostic): Meta =
      ImmutableMeta(
        _id, _loc, _scope, _parentId,
        _diagnostics :+ diagnostic,
        typ
      )

    override def withDiagnostics(diagnostics: Iterable[Diagnostic]): Meta =
      ImmutableMeta(
        _id, _loc, _scope, _parentId,
        _diagnostics ++ diagnostics,
        typ
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
    def scope: _Scope = {
      this.meta.scope.get match {
        case None => Scope.empty.asInstanceOf[_Scope]
        case Some(scope) => scope
      }
    }
  }

  case class Ident(
    meta: Meta,
    name: Name
  ) extends Node {
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
        case Declaration.TypeAlias(_, _, ident, kindAnnotation, rhs) =>
          val result: ListBuffer[Node] = ListBuffer(ident)
          kindAnnotation match {
            case Some(t) => result.append(t)
            case None => ()
          }
          rhs match {
            case Some(t) => result.append(t)
            case None => ()
          }
          result.toArray
        case Declaration.Module(_, _, ident, _, genericParams, body) =>
          (ident +: genericParams) ++ body
        case Declaration.Error(_, _) =>
          Array()
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
      pattern: Pattern,
      rhs: Option[Expression]
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

    case class Module(
      meta: Meta,
      modifiers: Set[Declaration.Modifier],
      ident: Ident,
      moduleScope: Scope,
      genericParams: Array[GenericParam],
      body: Array[Declaration]
    ) extends Declaration
    case class Error(
      override val meta: Meta,
      override val modifiers: Set[Declaration.Modifier]
    ) extends Declaration

    sealed trait Modifier {
      def typed: Typed.Declaration.Modifier = this.asInstanceOf[Typed.Declaration.Modifier]
    }
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
    override def children: Array[Node] =
      this match {
        case Pattern.Var(_, ident) => Array(ident)
        case Pattern.Annotated(_, inner, annotation) =>
          Array(inner, annotation)
        case Pattern.DotName(_, ident) => Array(ident)
        case Pattern.Paren(_, inner) => Array(inner)
        case Pattern.Annotated(_, inner, annotation) =>
          Array(inner, annotation)
        case Pattern.Error(_) => Array()
      }

    def withMeta(meta: Meta): Pattern = this match {
      case Pattern.Var(_, ident) => Pattern.Var(meta, ident)
      case Pattern.Annotated(_, inner, annotation) =>
        Pattern.Annotated(meta, inner, annotation)
      case Pattern.Paren(_, inner) =>
        Pattern.Paren(meta, inner)
      case Pattern.DotName(_, ident) =>
        Pattern.DotName(meta, ident)
      case Pattern.Function(_, f, args) =>
        Pattern.Function(meta, f, args)
    }
  }
  object Pattern {
    case class Var(
      meta: Meta,
      ident: Ident
    ) extends Pattern
    case class Annotated(
      meta: Meta,
      innerPattern: Pattern,
      typeAnnotation: TypeAnnotation
    ) extends Pattern
    case class DotName(
      meta: Meta,
      ident: Ident
    ) extends Pattern
    case class Function(
      meta: Meta,
      f: Pattern,
      params: Array[Param]
    ) extends Pattern
    case class Paren(
      meta: Meta,
      inner: Pattern
    ) extends Pattern
    case class Error(
      override val meta: Meta
    ) extends Pattern

    sealed trait Param extends Node {
      import Param._
      override def children: Array[Node] =
        this match {
          case Rest(_) => Array.empty
          case SubPattern(_, None, pattern) =>
            Array(pattern)
          case SubPattern(_, Some(ident), p) =>
            Array(ident, p)
        }
    }
    object Param {
      case class Rest(meta: Meta) extends Param
      case class SubPattern(
        meta: Meta,
        label: Option[Ident],
        pattern: Pattern
      ) extends Param
    }
  }

  sealed trait TypeAnnotation extends Node {
    import TypeAnnotation._
    override def children: Array[Node] =
      this match {
        case Var(_, ident) => Array(ident)
        case Forall(_, _, genericParams, body) =>
          genericParams :+ body
        case Func(_, _, from, to) =>
          from.flatMap({
            case (Some(ident), annotation) =>
              Array[Node](ident, annotation)
            case (None, annotation) =>
              Array[Node](annotation)
          }) :+ to

        case TApplication(_, tf, args) =>
          tf +: args
        case Paren(_, a) => Array(a)
        case Prop(_, e, prop) => Array(e, prop)
        case Error(_) => Array()
      }

    def withMeta(meta: Meta): TypeAnnotation = this match {
      case Var(_, ident) => Var(meta, ident)
      case Forall(_, scope, p, body) =>
        Forall(meta, scope, p, body)
      case Func(_, fs, gp, body) => Func(meta, fs, gp, body)
      case TApplication(_, f, args) => TApplication(meta, f, args)
      case Error(_) => Error(meta)
    }
  }
  object TypeAnnotation {
    case class Var(
      meta: Meta,
      ident: Ident
    ) extends TypeAnnotation
    case class TApplication(
      meta: Meta,
      tFunc: TypeAnnotation,
      args: Array[TypeAnnotation]
    ) extends TypeAnnotation
    case class Paren(
      meta: Meta,
      inner: TypeAnnotation
    ) extends TypeAnnotation
    case class Forall(
      meta: Meta,
      forallScope: _Scope,
      genericParams: Array[GenericParam],
      inner: TypeAnnotation
    ) extends TypeAnnotation
    case class Func(
      meta: Meta,
      funcScope: Scope,
      params: Array[(Option[Ident], TypeAnnotation)],
      returnType: TypeAnnotation
    ) extends TypeAnnotation
    case class Prop(
      meta: Meta,
      expr: Expression,
      prop: Ident
    ) extends TypeAnnotation
    case class Error(meta: Meta) extends TypeAnnotation
  }


  sealed trait Expression extends Node with Expression.Block.Member {
    import Expression._
    override def children: Array[Node] =
      this match {
        case Literal(_, _) => Array.empty
        case Var(_, i) => Array(i)
        case Call(_, expr, args) =>
          expr +: args
        case Func(_, _, _, genericParams, params, Some(returnType), body) =>
          genericParams ++ params.map(_.pattern) :+ returnType :+ body
        case Func(_, _, _, genericParams, params, None, body) =>
          genericParams ++ params.map(_.pattern) :+ body
        case Prop(_, e, p) => Array(e, p)
        case m: Module =>
          m.declarations.toArray
      }

    def withMeta(meta: Meta): Expression = this match {
      case Literal(_, a) => Literal(meta, a)
      case Var(_, a) => Var(meta, a)
      case c: Call => c.copy(meta)
      case f: Func => f.copy(meta)
      case b: Block => b.copy(meta)
      case m: Module => m.copy(meta)
      case i: If => i.copy(meta)
      case p: Prop => p.copy(meta)
      case p: Error => p.copy(meta)
      case w: With => w.copy(meta)
    }

    override def toString: String = this match {
      case Var(_, ident) => s"${ident.name.toString}, ${meta.loc}"
      case Prop(_, e, prop) => s"$e.$prop, ${meta.loc}"
      case _ => super.toString
    }
  }
  object Expression {
    case class Literal(
      meta: Meta,
      variant: Literal.Variant
    ) extends Expression
    case class Var(
      meta: Meta,
      ident: Ident
    ) extends Expression
    case class Func(
      meta: Meta,
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
    case class Module(
      meta: Meta,
      moduleScope: _Scope,
      declarations: Array[Declaration]
    ) extends Expression
    case class If(
      meta: Meta,
      predicate: Expression,
      trueBranch: Expression,
      falseBranch: Option[Expression]
    ) extends Expression
    case class Match(
      meta: Meta,
      expr: Expression,
      branches: Array[Match.Branch]
    ) extends Expression
    case class Call(
      meta: Meta,
      func: Expression,
      args: Array[Call.Arg]
    ) extends Expression
    case class Prop(
      meta: Meta,
      expr: Expression,
      prop: Ident
    ) extends Expression
    case class With(
      meta: Meta,
      e1: Expression,
      e2: Expression
    ) extends Expression
    case class Error(meta: Meta) extends Expression

    object Call {
      case class Arg(meta: Meta, label: Option[Ident], value: Expression) extends Node {
        override def children: Array[Node] =
          label match {
            case Some(ident) => Array(ident, value)
            case None => Array(value)
          }
      }
    }

    object Match {
      case class Branch(
        meta: Meta, branchScope: Scope, pattern: Pattern, expr: Expression
      ) extends Node {
        override def children: Array[Node] =
          Array(pattern, expr)
      }
    }

    object Literal {
      sealed trait Variant
      case class LInt(value: Int) extends Variant
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
      case Func(_, from, to) =>
        from :+ to
      case Error(_) => Array.empty
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
