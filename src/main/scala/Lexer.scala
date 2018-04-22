import IO._
import Tokens._
import Variant._
import java.nio.file.Path
import Utils._

object Lexer {
  def apply(path: Path, chars: Stream[Char]) = new Lexer(path, chars)
  def isIdentifierStarter(c: Char): Boolean = {
    (c >= 'A' && c <= 'Z') ||
      (c >= 'a' && c <= 'z') ||
      (c == '_')
  }
}

final class Lexer(
  path: Path,
  chars: Stream[Char]
) extends Stream[Token] {
  private var _line = 1
  private var _column = 1
  private var _currentChar = chars.next

  private var eofToken: Option[Token] = None

  def next: Token =
    currentChar match {
      case c if Lexer.isIdentifierStarter(c) =>
        identOrKeyword
      case c if c == EOF_CHAR =>
        eofToken match {
          case Some(tok) => tok
          case None =>
            val tok = makeToken(() => {
              advance
              (EOF, "")
            })
            eofToken = Some(tok)
            tok
        }
      case _ =>
        makeToken(() => {
          val lexeme = advance.toString
          (UNEXPECTED_CHAR, lexeme)
        })
    }

  private def identOrKeyword: Token = makeToken(() => {
    var lexeme = advance.toString
    while (Lexer.isIdentifierStarter(currentChar)) {
      lexeme += advance
    }
    (ID, lexeme)
  })


  private def makeToken(f: () => (Variant, String)): Token = {
    val start = this.pos
    val (variant, lexeme) = f()
    val stop = this.pos
    Token(variant, Loc(path, start, stop), lexeme)
  }


  private def advance: Char = {
    val c = currentChar
    if (c == '\n') {
      _line += 1
      _column = 0
    } else {
      _column += 1
    }
    this._currentChar = chars.next
    c
  }

  private def currentChar: Char = _currentChar
  private def pos: Pos = Pos(_line, _column)
}




