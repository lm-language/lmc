sealed trait Type

/**
  * A type constructor. E.g. `Int`, `Boolean`, `List`, etc
  * @param name
  */
case class Constructor(name: Symbol) extends Type
/**
  * A placeholder for a type that hasn't been inferred
  * yet.
  * @param id
  */
case class UnInferred(id: Int) extends Type

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
case class UnAssigned() extends Type
case class ErrorType() extends Type
