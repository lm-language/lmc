package lmc

import java.nio.file.Path

import syntax.token
import lmc.syntax.Parsed._
import token.Token
import token.Variant._
import diagnostics._
import io.Stream
import common.Loc
import common.ScopeBuilder

import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference

object Parser {

  def apply(path: Path, tokens: Stream[Token]) = new Parser(path, tokens)
  val RECOVERY_TOKENS = Set(
    NEWLINE, EOF, RPAREN, RBRACE
  )

  def isRecoveryToken(variant: token.Variant): Boolean = {
    RECOVERY_TOKENS.contains(variant)
  }
}

final class Parser(val path: Path, val tokens: Stream[Token]) {
  type Scope = syntax.Parsed._Scope
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

    val scope = common.ScopeBuilder(parent)
    _scopes match {
      case hd:: _ =>
        hd.get match {
          case Some(parentScope) =>
              parentScope.addChild(scope)
          case _ => ()
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

  private def expect(errors: collection.mutable.ListBuffer[Diagnostic])
      (variant: token.Variant): Token = {
    currentToken.variant match {
      case v if v == variant =>
        advance()
      case _ =>
        val loc = currentToken.loc.copy(end = currentToken.loc.start)
        errors.append(
          Diagnostic(
            severity = diagnostics.Severity.Error,
            loc = loc,
            variant = diagnostics.TokenExpected(variant.toString)))
        Token(
          loc = loc,
          variant = EXPECTED,
          lexeme = ""
        )
    }
  }

  def parseSourceFile(): SourceFile = withNewScope((scope) => {
    var declarations = Vector.empty[Declaration]
    val startToken = this.currentToken
    while (this.currentToken.variant != EOF) {
      declarations = declarations :+ parseDeclaration
    }
    val errors = collection.mutable.ListBuffer.empty[Diagnostic]
    val endToken = expect(errors)(EOF)
    val loc = Loc.between(startToken, endToken)
    val meta = Meta(
      loc,
      WeakReference(scope),
      errors
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
        val errors = collection.mutable.ListBuffer.empty[Diagnostic]
        expect(errors)(EQ)
        val expr = parseExpr()
        val stopTok = expect(errors)(SEMICOLON)
        Declaration(
          meta = Meta(
            loc = Loc.between(startTok, stopTok),
            scope = scope(),
            diagnostics = errors
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
      case FN =>
        val parentScope = scope()
        withNewScope(fnScope => {
          val startTok = advance()
          val errors = collection.mutable.ListBuffer.empty[Diagnostic]
          expect(errors)(LPAREN)
          expect(errors)(RPAREN)
          val annotation = currentToken.variant match {
            case COLON =>
              advance()
              Some(parseTypeAnnotation())
            case _ => None
          }
          expect(errors)(FATARROW)
          val body = parseExpr()
          Expr(
            meta = Meta(loc = Loc.between(startTok, body), scope = parentScope),
            typ = (),
            variant = Expr.Func(
              fnScope,
              List.empty,
              annotation,
              body
            )
          )
        })
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
    val pattern = currentToken.variant match {
      case ID =>
        val tok = advance()
        val meta = Meta(loc = tok.loc, scope())
        Pattern(
          meta = meta,
          typ = (),
          variant = Pattern.Var(Ident(meta, tok.lexeme))
        )
      case LPAREN =>
        advance()
        val p = parsePattern()
        val errors = collection.mutable.ListBuffer.empty[Diagnostic]
        expect(errors)(RPAREN)
        p.copy(meta = p.meta.copy(diagnostics = p.meta.diagnostics ++ errors))
      case _ =>
        val loc = currentToken.loc

        val skippedDiagnostics = recover()
        val diagnostics = List(Diagnostic(
          PatternExpected(),
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
          variant = Pattern.Error
        )
    }
    currentToken.variant match {
      case COLON =>
        advance()
        val annotation = parseTypeAnnotation()
        Pattern(
          meta = Meta(
            loc = Loc.between(pattern, annotation),
            scope()
          ),
          typ = (),
          variant = Pattern.Annotated(pattern, annotation)
        )
      case _ => pattern
    }
  }

  private def parseTypeAnnotation(): TypeAnnotation = {
     currentToken.variant match {
       case ID =>
         val tok = advance()
         val meta = Meta(
           loc = tok.loc,
           scope = scope()
         )
         val ident = Ident(
           meta = meta,
           name = tok.lexeme
         )
         TypeAnnotation(meta, TypeAnnotation.Var(ident))
       case _ =>
         val loc = currentToken.loc
         val skippedDiagnostics = recover()
         val diagnostics = List(Diagnostic(
            TypeExpected(),
            Severity.Error,
            loc
         ))
         addDiagnostics(diagnostics)
         addDiagnostics(skippedDiagnostics)
         TypeAnnotation(
           meta = Meta(
             loc = currentToken.loc,
             scope(),
             diagnostics ++ skippedDiagnostics
           ),
           variant = TypeAnnotation.Error
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
        Diagnostic(
          severity = Severity.Error,
          loc = tok.loc,
          variant = UnexpectedChar(tok.lexeme)
        )::diagnostics
      case INVALID_OPERATOR =>
          Diagnostic(
            severity = Severity.Error,
            loc = tok.loc,
            variant = InvalidOperator(tok.lexeme)
          )::diagnostics
      case _ => diagnostics
    }
  }
}