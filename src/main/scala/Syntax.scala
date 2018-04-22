object Syntax {

  case class Meta(
    loc: Loc
  ) extends HasLoc

  sealed trait Syntax {
    type Name
    type _Type
    type _Scope


    sealed trait Node extends HasLoc {
    }

    implicit class NodeOps(m: Meta) extends Node {
      def loc: Loc = m.loc
    }

    case class Ident(meta: Meta, name: Name) extends NodeOps(meta) with Node


    object Pattern {
      sealed trait Variant {
        type T = Variant
        case class Var(ident: Ident) extends T
      }
    }

    case class Pattern(
      meta: Meta,
      typ: _Type,
      variant: Pattern.Variant
    ) extends NodeOps(meta) with Node

    case class Binder(meta: Meta, pattern: Pattern) extends NodeOps(meta) with Node

    object Expr {
      sealed trait LiteralVariant {
        type T = LiteralVariant
        case class LInt(value: Int) extends T
      }
      sealed trait Variant {
        type T = Variant
        case class Var(ident: Ident) extends T
        case class Literal(variant: LiteralVariant) extends T
      }
    }
    final case class Expr(
      meta: Meta,
      typ: _Type,
      variant: Expr.Variant
    ) extends NodeOps(meta) with Node

    object Declaration {
      sealed trait Variant {
        type T = Variant
        case class Let(binder: Binder, rhs: Expr) extends T
      }
    }
    final case class Declaration(
      meta: Meta,
      variant: Declaration.Variant
    ) extends NodeOps(meta) with Node

    final case class SourceFile(
      meta: Meta,
      declarations: Iterable[Declaration],
      scope: _Scope
    ) extends NodeOps(meta) with Node
  }

  final object Parsed extends Syntax {
    type Name = String
    type _Type = Unit
    type _Scope = Unit
  }

  final object Typed extends Syntax {
    type Name = Symbol
    type _Type = Type
    type _Scope = Scope
  }
}