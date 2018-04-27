package lmc.syntax

import lmc.common.{HasLoc, Loc}

object token {
  object Variant extends Enumeration {
    type Variant = Value
    val
      ID, INT, EOF,
      // keywords
      LET, IF, ELSE, FN,

      // Punctuation
      VBAR, LPAREN, RPAREN, LBRACE, RBRACE, SEMICOLON,
      COLON,

      // Operators
      EQ, EQEQ, PLUS, FATARROW,

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
    lexeme: String
  ) extends HasLoc
}
