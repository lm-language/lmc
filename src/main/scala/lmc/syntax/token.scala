package lmc.syntax

import lmc.common.{HasLoc, Loc}

import scala.ref.WeakReference

object token {
  object Variant extends Enumeration {
    type Variant = Value
    val
      ID, INT, EOF,
      // keywords
      LET, IF, ELSE, FN, EXTERN, WITH, ENUM,
      TYPE, MODULE, ABSTRACT, OVERRIDE, INCLUDE,
      MATCH,

      // Punctuation
      VBAR, LPAREN, RPAREN, LBRACE, RBRACE, SEMICOLON,
      COLON, COMMA, LSQB, RSQB, TILDE,

      // Operators
      EQ, EQEQ, PLUS, FATARROW, ARROW, STAR, DOT, DOTDOT,
      BACKSLASH, SLASH,

      UNEXPECTED_CHAR,
      INVALID_OPERATOR,
      EXPECTED,
      NEWLINE
    = Value
  }
  type Variant = Variant.Variant
  final case class Token(
    variant: Variant,
    loc: Loc,
    lexeme: String,
    previous: Option[WeakReference[Token]]
  ) extends HasLoc
}
