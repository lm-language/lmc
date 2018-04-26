import IO._
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
  type Scope = Syntax.Parsed._Scope
  private var _currentToken: Token = tokens.next
  private var _lastToken: Token = _currentToken
  private def currentToken = _currentToken
  private var _errors: List[WeakReference[Diagnostic]] = List()
  private var _scopes = List.empty[WeakReference[Scope]]

  /**
   * advance the token stream by one character
   * and return the currentToken before advancing
   */
  private def advance(): Token = {
    val tok = currentToken
    _lastToken = _currentToken
    _currentToken = tokens.next
    while (currentToken.variant == NEWLINE) {
      _currentToken = tokens.next
    }
    tok
  }

  private def withNewScope[T](f: (ScopeBuilder) => T): T = {
    val startToken = currentToken
    val parent: WeakReference[Option[Scope]] = _scopes match {
      case hd :: _ =>
        WeakReference(hd.get)
      case _ => WeakReference(None)
    }

    val scope = ScopeBuilder(parent)
    _scopes match {
      case hd:: _ => {
        hd.get match {
          case Some(parentScope) =>
              parentScope.addChild(scope)
          case _ => ()
        }
      }
      case _ => ()
    }
    _pushScope(scope)
    val result = f(scope)
    _popScope()
    val loc = Loc.between(startToken, _lastToken)
    scope.setLoc(loc)
    result
  }

  private def _pushScope(scope: Scope): Unit = {
    this._scopes = WeakReference(scope)::_scopes
  }

  private def _popScope(): Unit = {
    _scopes = _scopes.tail
  }

  private def scope(): WeakReference[Scope] = {
    // Unsafe get because scope should definitely be
    // defined during parsing, otherwise, something
    // is wrong and exception should be thrown
    WeakReference(_scopes.head.get.get)
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

  def parseSourceFile(): SourceFile = withNewScope((scope) => {
    var declarations = Vector.empty[Declaration]
    val startToken = this.currentToken
    while (this.currentToken.variant != EOF) {
      declarations = declarations :+ parseDeclaration
    }
    val (endToken, diagnostics) = expect(EOF)
    val loc = Loc.between(startToken, endToken)
    val meta = Meta(
      loc,
      WeakReference(scope),
      diagnostics
    )
    SourceFile(
      meta,
      declarations,
      scope = scope
    )
  })

  def parseDeclaration: Declaration = {
    currentToken.variant match {
      case LET =>
        val startTok = advance()
        val pattern = parsePattern()
        val (_, diags1) = expect(EQ)
        val expr = parseExpr()
        val (stopTop, diags2) = expect(SEMICOLON)
        Declaration(
          meta = Meta(
            loc = Loc.between(startTok, stopTop),
            scope = scope,
            diagnostics = diags1 ++ diags2
          ),
          variant = Declaration.Let(pattern, expr)
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
            scope(),
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
          meta = Meta(loc = tok.loc, scope()),
          typ = (),
          variant = Expr.Literal(Expr.LInt(value))
        )
      case ID =>
        val tok = advance()
        val meta = Meta(loc = tok.loc, scope())
        Expr(
          meta = meta,
          typ = (),
          variant = Expr.Var(Ident(meta, tok.lexeme))
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
            scope(),
            diagnostics ++ skippedDiagnostics
          ),
          typ = (),
          variant = Expr.Error()
        )
    }
  }

  private def parsePattern(): Pattern = {
    currentToken.variant match {
      case ID =>
        val tok = advance()
        val meta = Meta(loc = tok.loc, scope())
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
            scope(),
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