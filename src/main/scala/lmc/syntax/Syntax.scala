package lmc.syntax

import lmc.common._
import lmc.diagnostics.Diagnostic

import scala.ref.WeakReference

trait Syntax {
  type Name
  type _Scope <: Scope
  type _Type
  type _Kind

  case class Meta(
    id: Int,
    loc: Loc,
    scope: WeakReference[_Scope],
    diagnostics: Iterable[Diagnostic] = List()
  ) extends HasLoc {
    def typed: lmc.syntax.Typed.Meta = this.asInstanceOf[Typed.Meta]
    def named: Named.Meta = this.asInstanceOf[Named.Meta]

    override def toString: String = ""
    def withDiagnostic(diagnostic: Diagnostic): Meta = {
      this.copy(diagnostics = this.diagnostics ++ List(diagnostic))
    }
    def withDiagnostics(diagnostics: Iterable[Diagnostic]): Meta = {
      this.copy(diagnostics = this.diagnostics ++ diagnostics)
    }
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

  sealed trait BlockMember extends Node

  implicit class NodeOps(m: Meta) extends HasMeta with HasLoc {
    def getMeta: Meta = m
    def loc: Loc = m.loc
  }

  case class Ident(
    meta: Meta, name: Name,
    duplicateBinder: Boolean = false
  ) extends NodeOps(meta) with Node {
    override def children: Iterable[Node] = List()

    override def toString: String = name.toString
  }

  object Pattern {

    type T = Variant
    case class Var(ident: Ident) extends T
    case class Annotated(pattern: Pattern, annotation: TypeAnnotation) extends T
    case class DotName(ident: Ident) extends T
    case class Function(pattern: Pattern, params: Vector[Param]) extends T
    case object Error extends T

    sealed trait Variant extends HasChildren {
      override def children: Iterable[Node] = this match {
        case Var(ident) => List(ident)
        case Annotated(pattern, annotation) =>
          List(pattern, annotation)
        case DotName(ident) => Vector(ident)
        case Function(pattern, params) => pattern +: params.flatMap({
          case Param.Rest(_) => Vector.empty[Node]
          case Param.SubPattern(_, Some(ident), p) =>
            Vector(ident, p)
          case Param.SubPattern(_, None, p) =>
            Vector(p)
        })
        case Error => List()
      }
    }

    sealed trait Param extends Node {
      override def toString: String = this match {
        case Param.Rest(_) => ".."
        case Param.SubPattern(_, Some(label), pat) =>
          s"$label = $pat"
        case Param.SubPattern(_, None, pat) =>
          pat.toString
      }

      override def children: Iterable[Node] = this match {
        case Param.Rest(_) => None
        case Param.SubPattern(_, Some(label), pat) =>
          List(label, pat)
        case Param.SubPattern(_, None,  pat) =>
          List(pat)
      }

    }
    object Param {
      case class Rest(meta: Meta) extends NodeOps(meta) with Param

      /**
        * @example
        *          label = .Option(x)
        */
      case class SubPattern(
        meta: Meta,
        label: Option[Ident],
        pattern: Pattern
      ) extends NodeOps(meta) with Param with Node

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
          s"(${p.toString}: ${t.toString})"
        case Pattern.DotName(ident) =>
          s".$ident"
        case Pattern.Function(p, params) =>
          s"$p($params)"
        case Pattern.Error => "<ERROR>"
      }
  }

  case class TypeAnnotation(
    meta: Meta,
    kind: _Kind,
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
    case class Func(
      params: Iterable[Param],
      returnType: TypeAnnotation
    ) extends T
    case class Forall(
      scope: _Scope,
      params: Iterable[GenericParam],
      annotation: TypeAnnotation
    ) extends T {
      override def toString: String =
        s"([$params] => $annotation)"
    }
    case class TApplication(
      tFunc: TypeAnnotation,
      args: Iterable[TypeAnnotation]
    ) extends T
    case class Prop(
      expr: Expr,
      prop: String
    ) extends T
    case object Error extends T
    sealed trait Variant extends HasChildren {
      override def children: Iterable[Node] =
        this match {
          case Var(ident) => List(ident)
          case TApplication(f, args) =>
            f::args.toList
          case Func(params, returnType) =>
            params.flatMap((param) => {
              param match {
                case (Some(ident), annotation) =>
                  List(ident, annotation)
                case (None, annotation) =>
                  List(annotation)
              }
            }) ++ List(returnType)
          case Prop(expr, _) =>
            List(expr)
          case Forall(_, params, annotation) =>
            params ++ List(annotation)
          case Error => List.empty
        }
    }
    type Param = (Option[Ident], TypeAnnotation)
  }

  object Expr {
    sealed trait LiteralVariant
    case class LInt(value: Int) extends LiteralVariant
    sealed trait Variant extends HasChildren {
      override def children: Iterable[Node] = this match {
        case Var(ident) => List(ident)
        case Literal(_) => List()
        case Module(_, decls) => decls
        case Func(_, _, genericParams, params, returnTypeAnnotation, body) =>
          val patterns = params.map(_.pattern)
          val ret = returnTypeAnnotation
              .map(x => List(x)).getOrElse(List())
          List(genericParams, patterns, ret, List(body)).flatten
        case Call(_, func, args) =>
          List(func) ++ args.map(_.value)
        case Prop(e, _) =>
          List(e)
        case WithExpression(e1, e2) =>
          List(e1, e2)
        case Block(_, members) => members
        case If(p, t, Some(f)) => List(p, t, f)
        case If(p, t, None) => List(p, t)
        case Match(e, branches) =>
          e.children ++ branches.flatMap(b => List(b.lhs, b.rhs))
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
      fnTok: lmc.syntax.token.Token,
      scope: _Scope,
      genericParams: Vector[GenericParam],
      params: Iterable[Param],
      returnTypeAnnotation: Option[TypeAnnotation],
      body: Expr
    ) extends T {
      override def toString: String =
        s"""fn $params:$returnTypeAnnotation => $body"""
    }
    case class Call(
      argsLoc: Loc,
      func: Expr,
      args: Vector[Arg]
    ) extends T
    case class Module(
      scope: _Scope,
      declarations: Iterable[Declaration]
    ) extends T
    case class Prop(
      expr: Expr,
      prop: String
    ) extends T
    case class WithExpression(
      e1: Expr,
      e2: Expr
    ) extends T
    case class Block(
      scope: _Scope,
      members: Vector[BlockMember]
    ) extends T
    case class If(
      predicate: Expr,
      trueBranch: Expr,
      falseBranch: Option[Expr]
    ) extends T
    case class Match(
      expr: Expr,
      branches: Vector[MatchBranch]
    ) extends T
    case class Error() extends T

    case class Arg(
      // label isn't a Name because the renamer
      // will have to know about the type of the function
      // that is being called to find the symbol for that label.
      // This would make the renamer intertwined with the type checker
      // so this is just a string. The type checker will resolve the
      // symbol using the type of the function.
      label: Option[(token.Token, String)],
      value: Expr
    ) {
      override def toString: String = label match {
        case Some((_, name)) =>
          s"$name = $value"
        case None => value.toString
      }
    }

    case class MatchBranch(
      scope: _Scope,
      lhs: Pattern,
      rhs: Expr
    )
  }

  case class Expr(
    meta: Meta,
    typ: _Type,
    variant: Expr.Variant
  ) extends NodeOps(meta) with Node with HasVariant[Expr.Variant] with BlockMember {
    override def children: Iterable[Node] = variant.children

    override def toString: String = variant.toString
  }

  object Declaration {
    sealed trait Variant extends HasChildren {
      override def children: Iterable[Node] = this match {
        case Let(pattern, rhsOpt) => rhsOpt match {
          case Some(rhs) => List(pattern, rhs)
          case None => List(pattern)
        }
        case TypeAlias(ident, kindAnnotation, typeAnnotation) =>
          List(ident, kindAnnotation.orNull, typeAnnotation.orNull)
            .filter(_ != null)
        case Include(e) =>
          List(e)
        case Enum(_, ident, genericParams, cases) =>
          Seq(Seq(ident), genericParams, cases).flatten
        case Error() => List()
      }
    }
    type T = Variant

    sealed trait Modifier

    object Modifier {
      case object Extern extends Modifier
      case object Abstract extends Modifier
      case object Override extends Modifier

      def named(m: Declaration.Modifier): Named.Declaration.Modifier =
        m match {
          case Modifier.Extern => Named.Declaration.Modifier.Extern
          case Modifier.Abstract => Named.Declaration.Modifier.Abstract
          case Modifier.Override => Named.Declaration.Modifier.Override
        }
      def typed(m: Declaration.Modifier): Typed.Declaration.Modifier =
        m match {
          case Modifier.Extern => Typed.Declaration.Modifier.Extern
          case Modifier.Abstract => Typed.Declaration.Modifier.Abstract
          case Modifier.Override => Typed.Declaration.Modifier.Override
        }
    }


    case class Let(pattern: Pattern, rhs: Option[Expr]) extends T {
      override def toString: String =
        s"let $pattern = $rhs"
    }
    case class TypeAlias(
      name: Ident,
      kindAnnotation: Option[KindAnnotation],
      rhs: Option[TypeAnnotation]
    ) extends T
    case class Include(
      expr: Expr
    ) extends T
    case class Enum(
      scope: _Scope,
      ident: Ident,
      genericParams: Vector[GenericParam],
      cases: Vector[EnumCase]
    ) extends T
    case class Error() extends T

  }

  case class EnumCase(
    meta: Meta,
    name: Ident,
    args: Vector[(Option[String], TypeAnnotation)]
  ) extends NodeOps(meta) with HasMeta with Node {
    override def children: Iterable[Node] =
      args.flatMap({
        case (_, annot) => List(annot)
      })
  }

  case class GenericParam(
    meta: Meta,
    kind: _Kind,
    ident: Ident,
    kindAnnotation: Option[KindAnnotation]
  ) extends NodeOps(meta) with HasMeta with Node {
    override def children: Iterable[Node] =
      kindAnnotation match {
        case Some(k) =>
          List(ident, k)
        case None =>
          List(ident)
      }
  }
  case class KindAnnotation(
    meta: Meta,
    variant: KindAnnotation.Variant
  ) extends NodeOps(meta) with HasMeta with Node {
    override def children: Iterable[Node] = variant.children
  }
  object KindAnnotation {
    type Variant = T
    sealed trait T extends HasChildren {
      override def children: Iterable[Node] =
        this match {
          case Star => List.empty
          case KFun(from, to) => List(from, List(to)).flatten
          case Error => List.empty
        }
    }
    case object Star extends T
    case class KFun(from: Iterable[KindAnnotation], to: KindAnnotation) extends T
    case object Error extends T
  }

  case class Declaration(
    meta: Meta,
    variant: Declaration.Variant,
    modifiers: Set[Declaration.Modifier]
  ) extends NodeOps(meta) with Node with HasVariant[Declaration.Variant] with BlockMember {
    override def children: Iterable[Node] = variant.children
    override def toString: String = variant.toString
    def isAbstract: Boolean = modifiers.contains(Declaration.Modifier.Abstract)
    def isOverride: Boolean = modifiers.contains(Declaration.Modifier.Override)
    def isExtern: Boolean = modifiers.contains(Declaration.Modifier.Extern)
  }

  case class SourceFile(
    meta: Meta,
    declarations: Iterable[Declaration],
    scope: _Scope
  ) extends NodeOps(meta) with Node {
    override def children: Iterable[Node] = declarations
  }
}
