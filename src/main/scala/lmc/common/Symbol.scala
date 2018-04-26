package lmc.common

case class Symbol(id: Int, text: String) {
  override def hashCode: Int = {
    id.hashCode
  }

  override def toString: String = text
}
