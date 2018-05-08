package lmc.types
import lmc.common.Symbol
import lmc.utils

sealed trait Type

/**
  * A primitive type. E.g. `Int`, `Boolean`, , etc
  */
object Primitive {
  case object Int extends Type
  case object Unit extends Type
  case object Bool extends Type
}

case object ErrorType extends Type
case class Var(symbol: Symbol) extends Type

/**
  * This type represents an as of un-inferred but
  * fixed type variable. This will be "instantiated"
  * to a type on use.
  * e.g. fn[A, B](a: A, b: B): A => A
  * Here, during type checking, A will be assigned
  * to Generic(n) and B to Generic(n + 1)
  * Every A in this scope would refer to Generic(n)
  * and that of B to Generic(n + 1).
  * Because A and B are different generic params,
  * Generic(n) is not assignable to Generic(n + 1)
  * and vice-versa.
  */
case class Existential(id: Int, text: String) extends Type {
  override def toString: String = s"Existential($text)"
}
case class Func(
  from: Vector[Func.Param],
  to: Type
) extends Type {
  override def toString: String = {
    s"""fn(${
      from.foldLeft("")((prev, current) => {
        val (symbolOpt, typ) = current
        symbolOpt match {
          case Some(symbol) =>
            prev
              .concat(",")
              .concat(symbol.text)
              .concat(":")
              .concat(typ.toString)
          case None =>
            prev
              .concat(",")
              .concat(typ.toString)
        }
      }).drop(1)
    })=>$to"""
  }
}
object Func {
  type Param = (Option[Symbol], Type)
}
case class Forall(params: Iterable[Symbol], typ: Type) extends Type {
  override def toString: String =
    s"forall [${utils.joinIterable(params)}] $typ"
}


