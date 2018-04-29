package lmc.types
import lmc.common.Symbol

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
case class Func(
  from: List[(Option[Symbol], Type)],
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
/**
  * An as of yet unbound type. This is assigned to all
  * bindings that will have been defined but not in current
  * scope. Used for producing better error messages. This
  * can distinguish between actually unbound values and
  * use before assignment errors.
  *
  * For e.g.
  * {
  *   // x: UnInferred
  *   // y: UnInferred
  *   let x = y; // error: Use before assignment
  *   let p = asdf; // error: Unbound variable asdf
  *   ...
  *   let y = 4
  *   // y: Int
  *   ...
  * }
  */
case object UnAssigned extends Type

/**
  * A placeholder for a type that hasn't been inferred
  * yet.
 *
  * @param id
  */
case class UnInferred(id: Int) extends Type
