package lmc.common

case class Symbol(id: Int, text: String) {
  override def hashCode: Int = {
    id.hashCode
  }

  override def toString: String = text

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case other@Symbol(_, _) =>
        other.id == id
      case _ =>
        false
    }
}
