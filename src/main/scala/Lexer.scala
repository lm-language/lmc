import IO._
import Tokens._
import java.nio.file.{Path}

object Lexer {
  def apply(path: Path, chars: Stream[Char]) = new Lexer(path, chars)
}

final class Lexer(
  path: Path,
  chars: Stream[Char]
) extends Stream[Token] {
  private var _line = 1
  private var _column = 1
  def next: Token = ???
}