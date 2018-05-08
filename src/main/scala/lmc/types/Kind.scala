package lmc.types

sealed trait Kind {
  override def toString: String = this match {
    case Kind.Star => "*"
    case Kind.KFun(from, to) => s"([$from] => $to)"
  }
}
object Kind {
  case object Star extends Kind
  case class KFun(from: Kind, to: Kind) extends Kind
}
