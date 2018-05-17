package lmc.common

case class Pos(line: Int, column: Int) {
  def lt(other: Pos): Boolean = {
    if (this.line == other.line) {
      this.column < other.column
    } else {
      this.line < other.line
    }
  }

  def <(that: Pos): Boolean = lt(that)
  def <=(that: Pos): Boolean = that == this || lt(that)
  def >(that: Pos): Boolean = !(this <= that)
  def >=(that: Pos): Boolean = that == this || this > that
}
