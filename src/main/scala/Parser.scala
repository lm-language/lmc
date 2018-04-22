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
  private def advance(): Token = {
    val tok = currentToken
    _currentToken = tokens.next
    while (currentToken.variant == NEWLINE) {
      _currentToken = tokens.next
    }
    tok
  }

  private def expect(variant: Tokens.Variant): (Token, Iterable[Diagnostic]) = {
    currentToken.variant match {
      case v if v == variant =>
        (advance(), List.empty)
      case _ =>
        val loc = currentToken.loc.copy(end = currentToken.loc.start)
        (Token(
          loc = loc,
          variant = EXPECTED,
          lexeme = ""
        ), List(Diagnostic(
          severity = Diagnostics.Severity.Error,
          loc = loc,
          variant = Diagnostics.TokenExpected(variant.toString)))
        )
    }
  }

  def parseSourceFile(): SourceFile = {
    var declarations = Vector.empty[Declaration]
    val startToken = this.currentToken
    while (this.currentToken.variant != EOF) {
      declarations = declarations :+ parseDeclaration
    }
    val (endToken, diagnostics) = expect(EOF)
    SourceFile(
      Meta(Loc.between(startToken, endToken), diagnostics = diagnostics),
      declarations,
      scope = ()
    )
  }

  def parseDeclaration: Declaration = {
    currentToken.variant match {
      case LET =>
        val startTok = advance()
        val binder = parseBinder()
        val (_, diags1) = expect(EQ)
        val expr = parseExpr()
        val (stopTop, diags2) = expect(SEMICOLON)
        Declaration(
          meta = Meta(
            loc = Loc.between(startTok, stopTop),
            diagnostics = diags1 ++ diags2
          ),
          variant = Declaration.Let(binder, expr)
        )
      case _ =>
        val loc = currentToken.loc

        val skippedDiagnostics = recover()
        val diagnostics = List(Diagnostic(
          DeclarationExpected(),
          Severity.Error,
          loc
        ))
        addDiagnostics(diagnostics)
        addDiagnostics(skippedDiagnostics)
        Declaration(
          meta = Meta(
            loc = currentToken.loc,
            diagnostics ++ skippedDiagnostics
          ),
          variant = Declaration.Error()
        )
    }
  }

  private def parseExpr(): Expr = {
    currentToken.variant match {
      case INT =>
        val tok = advance()
        val value = tok.lexeme.replaceAll("_", "").toInt
        Expr(
          meta = Meta(loc = tok.loc),
          variant = Expr.Literal(Expr.LInt(value)),
          typ = ()
        )
      case _ =>
        val loc = currentToken.loc

        val skippedDiagnostics = recover()
        val diagnostics = List(Diagnostic(
          ExpressionExpected(),
          Severity.Error,
          loc
        ))
        addDiagnostics(diagnostics)
        addDiagnostics(skippedDiagnostics)
        Expr(
          meta = Meta(
            loc = currentToken.loc,
            diagnostics ++ skippedDiagnostics
          ),
          typ = (),
          variant = Expr.Error()
        )
    }
  }

  private def parseBinder(): Binder = {
    val pattern = parsePattern()
    Binder(meta = Meta(
      loc = pattern.loc
    ), pattern = pattern)
  }

  private def parsePattern(): Pattern = {
    currentToken.variant match {
      case ID =>
        val tok = advance()
        val meta = Meta(loc = tok.loc)
        Pattern(
          meta = meta,
          typ = (),
          variant = Pattern.Var(Ident(meta, tok.lexeme))
        )
      case _ =>
        val loc = currentToken.loc

        val skippedDiagnostics = recover()
        val diagnostics = List(Diagnostic(
          ExpressionExpected(),
          Severity.Error,
          loc
        ))
        addDiagnostics(diagnostics)
        addDiagnostics(skippedDiagnostics)
        Pattern(
          meta = Meta(
            loc = currentToken.loc,
            diagnostics ++ skippedDiagnostics
          ),
          typ = (),
          variant = Pattern.Error()
        )
    }
  }

  private def addDiagnostics(diagnostics: Iterable[Diagnostic]): Unit = {
    for (d <- diagnostics) {
      _errors = new WeakReference[Diagnostic](d)::_errors
    }
  }

  private def recover(): List[Diagnostic] = {
    var diagnostics = List.empty[Diagnostic]
    diagnostics = consTokenDiagnostics(diagnostics, advance())
    while (!Parser.isRecoveryToken(currentToken.variant)) {
      val tok = advance()
      diagnostics = consTokenDiagnostics(diagnostics, tok)
    }
    diagnostics.reverse
  }

  private def consTokenDiagnostics(diagnostics: List[Diagnostic], tok: Token): List[Diagnostic] = {
    tok.variant match {
      case UNEXPECTED_CHAR =>
        Diagnostics.Diagnostic(
          severity = Diagnostics.Severity.Error,
          loc = tok.loc,
          variant = UnexpectedChar(tok.lexeme)
        )::diagnostics
      case INVALID_OPERATOR =>
          Diagnostics.Diagnostic(
            severity = Diagnostics.Severity.Error,
            loc = tok.loc,
            variant = InvalidOperator(tok.lexeme)
          )::diagnostics
      case _ => diagnostics
    }
  }
}