import java.nio.file.Path

case class Pos(line: Int, column: Int)
case class Loc(
  path: Path,
  start: Pos,
  end: Pos
)

trait HasLoc {
  def loc: Loc
}


object Loc {
  def between(start: HasLoc, end: HasLoc): Loc =
    start.loc.copy(end = end.loc.end)
}
  
