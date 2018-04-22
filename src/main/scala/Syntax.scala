object Syntax {

  case class Meta(
    loc: Loc,
    diagnostics: Iterable[Diagnostics.Diagnostic] = List()
  ) extends HasLoc

  sealed trait Syntax {
    type Name
    type _Type
    type _Scope


    sealed trait HasMeta {
      def getMeta: Meta
    }

    trait HasChildren {
      def children: Iterable[Node]
    }
    sealed trait Node extends HasLoc with HasMeta with HasChildren {
      def errors: Iterable[Diagnostics.Diagnostic] = {
        this.getMeta.diagnostics ++ this.children.flatMap(_.errors)
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
      sealed trait Variant extends HasChildren {
        type T = Variant
        case class Var(ident: Ident) extends T

        override def children: Iterable[Node] = this match {
          case Var(ident) => List(ident)
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

    case class Binder(meta: Meta, pattern: Pattern) extends NodeOps(meta) with Node {
      override def children: Iterable[Node] = List(pattern)
    }

    object Expr {
      sealed trait LiteralVariant {
        type T = LiteralVariant
        case class LInt(value: Int) extends T
      }
      sealed trait Variant extends HasChildren {
        type T = Variant
        case class Var(ident: Ident) extends T
        case class Literal(variant: LiteralVariant) extends T

        override def children: Iterable[Node] = this match {
          case Var(ident) => List(ident)
          case Literal(_) => List()
        }
      }
    }
    final case class Expr(
      meta: Meta,
      typ: _Type,
      variant: Expr.Variant
    ) extends NodeOps(meta) with Node {
      override def children: Iterable[Node] = variant.children
    }

    object Declaration {
      sealed trait Variant extends HasChildren {
        override def children: Iterable[Node] = this match {
          case Let(binder, rhs) => List(binder, rhs)
          case Error() => List()
        }
      }
      type T = Variant
      case class Let(binder: Binder, rhs: Expr) extends T
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