import IO._
import Syntax.Meta
import Syntax.Parsed._
import java.nio.file.Path
import Diagnostics._
import Tokens._
import Diagnostics.Diagnostic
import scala.ref.WeakReference

object Parser {
  import Variant._

  def apply(path: Path, tokens: Stream[Token]) = new Parser(path, tokens)
  val RECOVERY_TOKENS = Set(
    NEWLINE, EOF, RPAREN, RBRACE
  )

  def isRecoveryToken(variant: Variant): Boolean = {
    RECOVERY_TOKENS.contains(variant)
  }
}

final class Parser(val path: Path, val tokens: Stream[Token]) {
  import Tokens.Variant._
  private var _currentToken: Token = tokens.next
  private def currentToken = _currentToken
  private var _errors: List[WeakReference[Diagnostic]] = List()

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

  def parseSourceFile(): SourceFile = {
    var declarations = Vector.empty[Declaration]
    val startToken = this.currentToken
    while (this.currentToken.variant != EOF) {
      declarations = declarations :+ parseDeclaration
    }
    val endToken = expect(EOF)
    SourceFile(
      Meta(Loc.between(startToken, endToken)),
      declarations,
      scope = ()
    )
  }

  def parseDeclaration: Declaration = {
    currentToken.variant match {
      case _ =>
        val diagnostics = List(Diagnostic(
          DeclarationExpected(),
          Severity.Error,
          currentToken.loc
        ))
        addDiagnostics(diagnostics)
        recover()
        Declaration(
          meta = Meta(
            loc = currentToken.loc,
            diagnostics
          ),
          variant = Declaration.Error()
        )
    }
  }

  private def addDiagnostics(diagnostics: Iterable[Diagnostic]): Unit = {
    for (d <- diagnostics) {
      _errors = new WeakReference[Diagnostic](d)::_errors
    }
  }

  private def recover(): Unit = {
    while (!Parser.isRecoveryToken(currentToken.variant)) {
      advance()
    }
  }
}