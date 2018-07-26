package lmc.common

import lmc.utils.Debug

case class Symbol(
  id: Int, text: String,
  members: collection.mutable.HashMap[String, Symbol] = collection.mutable.HashMap.empty
) {
  override def hashCode: Int = {
    id.hashCode
  }

  override def toString: String = if (Debug.isDebugging)
    s"$text:$id"
  else text

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case other@Symbol(_, _, _) =>
        other.id == id
      case _ =>
        false
    }
}
