sealed trait Type {
  case class Constructor(name: Symbol) extends Type
}
