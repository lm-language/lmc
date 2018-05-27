package lmc

import java.nio.file.Path

import syntax.token._
import Variant._
import io._
import lmc.common.{Loc, Pos}

object Lexer {
  def apply(path: Path, chars: Stream[Char]) = new Lexer(path, chars)
  def isIdentifierStarter(c: Char): Boolean = {
    (c >= 'A' && c <= 'Z') ||
      (c >= 'a' && c <= 'z') ||
      (c == '_')
  }

  def isIdentifierChar(c: Char): Boolean = {
    isIdentifierStarter(c) || isDigit(c)
  }

  def isDigit(c: Char): Boolean =
    c >= '0' && c <= '9'

  val KEYWORD_TOKENS = Map(
    "let" -> LET,
    "fn" -> FN,
    "extern" -> EXTERN,
    "abstract" -> ABSTRACT,
    "override" -> OVERRIDE,
    "module" -> MODULE,
    "type" -> TYPE,
    "include" -> INCLUDE,
    "with" -> WITH,
    "if" -> IF,
    "else" -> ELSE,
    "enum" -> ENUM,
    "match" -> MATCH
  )
  val OPERATOR_TOKENS = Map(
    "==" -> EQEQ,
    "=>" -> FATARROW,
    "=" -> EQ,
    "+" -> PLUS,
    "*" -> STAR,
    "." -> DOT,
    ".." -> DOTDOT
  )
  val PUNCTUATION_TOKENS = Map(
    '\n' -> NEWLINE,
    '(' -> LPAREN,
    ')' -> RPAREN,
    '{' -> LBRACE,
    '}' -> RBRACE,
    ';' -> SEMICOLON,
    ':' -> COLON,
    ',' -> COMMA,
    '[' -> LSQB,
    ']' -> RSQB
  )

  val PUNCTUATION_CHARS: Set[Char] =
    PUNCTUATION_TOKENS.keySet

  val OPERATOR_CHARS: Set[Char] =
      OPERATOR_TOKENS.keysIterator.flatMap(_.iterator).toSet

  // Ensure that PUNCTUATION_CHARS and OPERATOR_CHARS don't have any
  // common characters
  PUNCTUATION_CHARS.foreach(c => {
    assert(!OPERATOR_CHARS.contains(c), s"""Character '$c' is a member of both PUNCTUATION_CHARS and OPERATOR_CHARS""")
  })
  OPERATOR_CHARS.foreach(c => {
    assert(!PUNCTUATION_CHARS.contains(c), s"""Character '$c' is a member of both PUNCTUATION_CHARS and OPERATOR_CHARS""")
  })
}

final class Lexer(
  path: Path,
  chars: Stream[Char]
) extends Stream[Token] {
  private var _line = 1
  private var _column = 1
  private var _currentChar = chars.next
  private var _previousLine = 1
  private var _previousCol  = 1

  private var eofToken: Option[Token] = None

  def next: Token = {
    while (currentChar == ' ' || currentChar == '\r' || currentChar == '\t') {
      advance
    }
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
      case c if Lexer.OPERATOR_CHARS.contains(c) =>
          operator
      case c if Lexer.isDigit(c) =>
        number
      case c =>
        Lexer.PUNCTUATION_TOKENS.get(c) match {
          case Some(variant) =>
            makeToken(() => {
              val lexeme = advance.toString
              (variant, lexeme)
            })
          case _ =>
            makeToken(() => {
              val lexeme = advance.toString
              (UNEXPECTED_CHAR, lexeme)
            })
        }

    }
  }

  private def operator: Token = makeToken(() => {
    var lexeme = advance.toString
    while (Lexer.OPERATOR_CHARS.contains(this.currentChar)) {
      lexeme += advance
    }
    val variant = Lexer.OPERATOR_TOKENS.get(lexeme) match {
      case Some(v) => v
      case None => INVALID_OPERATOR
    }
    (variant, lexeme)
  })

  private def number: Token = makeToken(() => {
    var lexeme = advance.toString
    while (Lexer.isDigit(currentChar) || currentChar == '_') {
      lexeme += advance
    }
    (INT, lexeme)
  })

  private def identOrKeyword: Token = makeToken(() => {
    var lexeme = advance.toString
    while (Lexer.isIdentifierChar(currentChar)) {
      lexeme += advance
    }
    val variant = Lexer.KEYWORD_TOKENS.get(lexeme) match {
      case Some(v) => v
      case _ => ID
    }
    (variant, lexeme)
  })



  private def makeToken(f: () => (Variant, String)): Token = {
    val start = this.pos
    val (variant, lexeme) = f()
    val stop = Pos(_previousLine, _previousCol)
    Token(variant, Loc(path, start, stop), lexeme)
  }

  private def advance: Char = {
    val c = currentChar
    _previousCol = _column
    _previousLine = _line
    if (c == '\n') {
      _line += 1
      _column = 1
    } else {
      _column += 1
    }
    this._currentChar = chars.next
    c
  }

  private def currentChar: Char = _currentChar
  private def pos: Pos = Pos(_line, _column)
}




