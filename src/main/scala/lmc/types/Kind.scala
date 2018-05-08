package lmc.types

sealed trait Kind {
  override def toString: String = this match {
    case Kind.Star => "*"
  }
}
object Kind {
  case object Star extends Kind
}
