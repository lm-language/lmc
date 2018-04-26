package lmc.common

import java.nio.file.Path

case class Loc(
  path: Path,
  start: Pos,
  end: Pos
)

object Loc {
  def between(start: HasLoc, end: HasLoc): Loc =
    start.loc.copy(end = end.loc.end)
}