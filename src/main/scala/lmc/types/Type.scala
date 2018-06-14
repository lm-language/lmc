package lmc.types
import lmc.common.Symbol
import lmc.utils

sealed trait Type

case object ErrorType extends Type
case class Var(symbol: Symbol) extends Type

case class Module(
  types: Map[Symbol, Kind],
  values: Map[Symbol, Type]
) extends Type {
  val kindOfString: Map[String, Kind] =
    types.map(entry => entry._1.text -> entry._2)
  val typeOfString: Map[String, Type] =
    values.map(entry => entry._1.text -> entry._2)
  val symbolOfString: Map[String, Symbol] =
    types.map(entry => entry._1.text -> entry._1) ++
    values.map(entry => entry._1.text -> entry._1)
}

case class Abstract(
  abstractTypes: Map[Symbol, Kind],
  abstractValues: Map[Symbol, Type],
  innerType: Type
) extends Type

// a type constructor; Like a type variable but is not substitutable
// or assignable to any other type constructor
case class Constructor(symbol: Symbol, kind: Kind) extends Type {
  override def toString: String = symbol.text
}
case class TApplication(tFunc: Type, arg: Type) extends Type {
  override def toString: String = s"$tFunc[$arg]"
}

/**
  * This type represents an instance of an existentially quantified
  * type variable.
  * For example, when we're checking a type [A] => fn(A) => A against fn(Int) => Int,
  * we first instantiate the forall type by creating new ExistentialInstance
  * for each forall param and substituting it in the body so,
  * after instantiation, [A] => fn(A) => A becomes
  * fn(ExistentialInstance(n, "A")) => ExistentialInstance(n, "A"),
  * where n is a freshly generated unique Int.
  * Then we recursively check the two types for equality of types.
  *
  * When ExistentialInstance is compared with another type,
  * if the instance hasn't yet been assigned another type in
  * context, an assignment is made.
  * If it is already assigned then we compare the assigned type
  * with the other type.
  *
  * Two instances can only match if both their ids are equal.
  */
case class ExistentialInstance(id: Int, text: String) extends Type {
  override def toString: String = s"Existential($text)"
}
case class Func(
  from: Array[Func.Param],
  to: Type
) extends Type {
  override def toString: String = {
    s"""(fn(${
      from.foldLeft("")((prev, current) => {
        val (symbolOpt, typ) = current
        symbolOpt match {
          case Some(symbol) =>
            prev
              .concat(", ")
              .concat(symbol.text)
              .concat(":")
              .concat(typ.toString)
          case None =>
            prev
              .concat(", ")
              .concat(typ.toString)
        }
      }).drop(2)
    }) => $to)"""
  }
}
object Func {
  type Param = (Option[Symbol], Type)
}
case class Forall(param: Symbol, typ: Type) extends Type {
  override def toString: String = {
    val inner = _innerTyp(this)
    val params = _params(this)
    s"([${utils.joinIterable(params)}] => $inner)"
  }

  private def _params(t: Type): List[Symbol] = {
    t match {
      case Forall(p, t1) => p::_params(t1)
      case _ => List.empty
    }
  }

  private def _innerTyp(t: Type): Type = {
    t match {
      case Forall(_, t1) => _innerTyp(t1)
      case _ => t
    }
  }
}
case object Uninferred extends Type

sealed trait Variance
case object CoVariance
case object ContraVariance
case object InVariance

