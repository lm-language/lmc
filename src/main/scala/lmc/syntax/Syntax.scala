package lmc.syntax

import lmc.common._
import lmc.utils.joinIterable
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
    def typed(typ: lmc.Value.Type): Typed.Meta =
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
    override def toString =
      name.toString
  }

  sealed trait Declaration extends Node with Term.Block.Member {
    override def toString = this match {
      case Declaration.Let(_, pattern, rhs) =>
        s"let $pattern = $rhs"
    }
    override def children: Array[Node] =
      this match {
        case Declaration.Let(_, ident, Some(rhs)) =>
          Array(ident, rhs)
        case Declaration.Let(_, ident, None) =>
          Array(ident)
        case Declaration.TypeAlias(_, ident, kindAnnotation, rhs) =>
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
        case Declaration.Module(_, ident, _, body) =>
          ident +: body
        case Declaration.Error(_) =>
          Array()
      }
  }

  object Declaration {
    case class Let(
      override val meta: Meta,
      pattern: Pattern,
      rhs: Option[Term]
    ) extends Declaration

    case class TypeAlias(
      override val meta: Meta,
      ident: Ident,
      kindAnnotation: Option[KindAnnotation],
      rhs: Option[Term]
    ) extends Declaration

    case class Include(
      override val meta: Meta,
      expr: Term
    ) extends Declaration

    case class Enum(
      override val meta: Meta,
      enumScope: _Scope,
      ident: Ident,
      cases: Array[Enum.Case]
    ) extends Declaration

    case class Module(
      meta: Meta,
      ident: Ident,
      moduleScope: Scope,
      body: Array[Declaration]
    ) extends Declaration
    case class Error(
      override val meta: Meta,
    ) extends Declaration

    object Enum {
      case class Case(
        override val meta: Meta,
        ident: Ident,
        params: Array[(Option[Ident], Term)]
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
    override def toString = this match {
      case Pattern.Var(_, ident) => ident.name.toString
      case Pattern.Annotated(_, inner, annotation) =>
          s"$inner: $annotation"
    }
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
      typeAnnotation: Term
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


  sealed trait Term extends Node with Term.Block.Member {
    import Term._
    override def children: Array[Node] =
      this match {
        case Literal(_, _) => Array.empty
        case Var(_, i) => Array(i)
        case Call(_, expr, args) =>
          expr +: args
        case Func(_, _, _, params, Some(returnType), body) =>
          params.map(_.pattern) :+ returnType :+ body
        case Func(_, _, _, params, None, body) =>
          params.map(_.pattern) :+ body
        case Prop(_, e, p) => Array(e, p)
        case m: Module =>
          m.declarations.toArray
        case Forall(_, _, body) =>
          Array(body)
        case Arrow(_, _, Some(label), from, to) =>
          Array(label, from, to)
        case Arrow(_, _, None, from, to) =>
          Array(from, to)
        case TApplication(_, tf, args) =>
            tf +: args
        case If(_, p, t, Some(f)) =>
          Array(p, t, f)
        case If(_, p, t, None) =>
          Array(p, t)
        case Error(_) => Array()
      }

    def withMeta(meta: Meta): Term = this match {
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
      case a: Arrow => a.copy(meta)
      case f: Forall => f.copy(meta)
    }

    override def toString: String = this match {
      case Literal(_, Literal.LInt(x)) => x.toString
      case Var(_, ident) => ident.name.toString
      case Prop(_, e, prop) => s"$e.$prop, ${meta.loc}"
      case Func(_, _, _, params, Some(returnType), body) =>
        s"fn($params): $returnType => $body"
      case Func(_, _, _, params, None, body) =>
        s"fn(${joinIterable(params)}) => $body"
      case Arrow(_, _, Some(label), from, to) =>
        s"(~($label: $from) -> $to)"
      case Arrow(_, _, None, from, to) =>
        s"($from -> $to)"
      case If(_, cond, t, Some(f)) =>
        s"if ($cond) $t else $f"
      case If(_, cond, t, None) =>
        s"if ($cond) $t"
      case Call(_, f, args) =>
        s"$f(${joinIterable(args)})"
    }
  }
  object Term {
    case class Literal(
      meta: Meta,
      variant: Literal.Variant
    ) extends Term
    case class Var(
      meta: Meta,
      ident: Ident
    ) extends Term
    case class Func(
      meta: Meta,
      fnToken: Token,
      funcScope: _Scope,
      params: Array[Param],
      returnType: Option[Term],
      body: Term
    ) extends Term
    case class Block(
      override val meta: Meta,
      blockScope: _Scope,
      members: Array[Block.Member]
    ) extends Term
    case class Module(
      meta: Meta,
      moduleScope: _Scope,
      declarations: Array[Declaration]
    ) extends Term
    case class If(
      meta: Meta,
      predicate: Term,
      trueBranch: Term,
      falseBranch: Option[Term]
    ) extends Term
    case class Match(
      meta: Meta,
      expr: Term,
      branches: Array[Match.Branch]
    ) extends Term
    case class Call(
      meta: Meta,
      func: Term,
      args: Array[Call.Arg]
    ) extends Term
    case class Prop(
      meta: Meta,
      expr: Term,
      prop: Ident
    ) extends Term
    case class With(
      meta: Meta,
      e1: Term,
      e2: Term
    ) extends Term
    case class Forall(
      meta: Meta,
      forallScope: _Scope,
      inner: Term
    ) extends Term
    case class Arrow(
      meta: Meta,
      funcScope: Scope,
      label: Option[Ident],
      from: Term,
      returnType: Term
    ) extends Term
    case class TApplication(
      meta: Meta,
      tFunc: Term,
      args: Array[Term]
    ) extends Term
    case class Error(meta: Meta) extends Term

    object Call {
      case class Arg(meta: Meta, label: Option[Ident], value: Term) extends Node {
        override def toString =
          value.toString
        override def children: Array[Node] =
          label match {
            case Some(ident) => Array(ident, value)
            case None => Array(value)
          }
      }
    }

    object Match {
      case class Branch(
        meta: Meta, branchScope: Scope, pattern: Pattern, expr: Term
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
    ) {
      override def toString: String = pattern.toString
    }
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
