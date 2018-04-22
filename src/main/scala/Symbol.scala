case class Symbol(id: Int, text: String) {
  override def hashCode: Int =
    id.hashCode
}
