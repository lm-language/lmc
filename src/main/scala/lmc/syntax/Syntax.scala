package lmc.syntax

import lmc.common._
import lmc.diagnostics.Diagnostic
import lmc.utils

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
      prop: Ident
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
          case Prop(expr, ident) =>
            List(expr, ident)
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
        case Prop(e, ident) =>
          List(e, ident)
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
      prop: Ident
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
        case ExternLet(name, annotation) =>
          List(name, annotation)
        case TypeAlias(ident, kindAnnotation, typeAnnotation) =>
          kindAnnotation match {
            case Some(k) => List(ident, k, typeAnnotation)
            case None => List(ident, typeAnnotation)
          }
        case ExternType(ident, kindAnnotation) =>
          kindAnnotation match {
            case Some(k) => List(ident, k)
            case None => List(ident)
          }
        case Existential(ident, kindAnnotation) =>
          kindAnnotation match {
            case Some(k) => List(ident, k)
            case None => List(ident)
          }
        case Error() => List()
      }
    }
    type T = Variant
    case class Let(pattern: Pattern, rhs: Expr) extends T {
      override def toString: String =
        s"let $pattern = $rhs"
    }
    case class ExternLet(name: Ident, typeAnnotation: TypeAnnotation) extends T
    case class ExternType(name: Ident, kindAnnotation: Option[KindAnnotation]) extends T
    case class TypeAlias(
      name: Ident,
      kindAnnotation: Option[KindAnnotation],
      typeAnnotation: TypeAnnotation
    ) extends T
    case class Existential(
      name: Ident,
      kindAnnotation: Option[KindAnnotation]
    ) extends T
    case class Error() extends T

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
    variant: Declaration.Variant
  ) extends NodeOps(meta) with Node with HasVariant[Declaration.Variant] {
    override def children: Iterable[Node] = variant.children
    override def toString: String = variant.toString
  }

  case class SourceFile(
    meta: Meta,
    declarations: Iterable[Declaration],
    scope: _Scope
  ) extends NodeOps(meta) with Node {
    override def children: Iterable[Node] = declarations
  }
}
