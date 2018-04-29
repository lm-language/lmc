package lmc.common

case class Pos(line: Int, column: Int) {
  def lt(other: Pos): Boolean = {
    if (this.line == other.line) {
      this.column < other.column
    } else {
      this.line < other.line
    }
  }
}
