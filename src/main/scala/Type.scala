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
  * Represents a type that hasn't been bound yet.
  * before type checking a module or source file,
  * all bindings in its scope are added to the
  * symbol table of the scope with this type.
  * After inference and checking of each declaration
  * in the module, this is replaced with a more specific
  * type
  */
case class UnBound() extends Type
