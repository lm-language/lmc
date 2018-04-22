import Utils._
import IO._
import Syntax.Parsed._
import java.nio.file.{Path}
import Tokens.Token
import scala.collection.mutable
import Loc._
import Diagnostics.Diagnostic

object Parser {
  def apply(path: Path, tokens: Stream[Token]) = new Parser(path, tokens)
}

final class Parser(val path: Path, val tokens: Stream[Token]) {
  private var _currentToken: Token = tokens.next
  private def currentToken = _currentToken
  private var _errors: List[Diagnostic] = List()
  private def errorAtLoc(loc: Loc, variant: Diagnostics.Variant) = {
    _errors = Diagnostic(variant, Diagnostics.Severity.Error, loc)::_errors
  }

  /**
   * advance the token stream by one character
   * and return the currentToken before advancing
   */
  private def advance(skipNewlines: Boolean = true): Token = {
    val tok = currentToken
    _currentToken = tokens.next
    tok
  }

  private def expect(variant: Tokens.Variant): Token = {
    currentToken.variant match {
      case v if v == variant =>
        advance()
      case _ =>
        ???
    }
  }

  def parseSourceFile(): SourceFile = ???
}