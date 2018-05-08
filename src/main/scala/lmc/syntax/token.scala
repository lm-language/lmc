package lmc.syntax

import lmc.common.{HasLoc, Loc}

object token {
  object Variant extends Enumeration {
    type Variant = Value
    val
      ID, INT, EOF,
      // keywords
      LET, IF, ELSE, FN, EXTERN, FORALL,
      TYPE,

      // Punctuation
      VBAR, LPAREN, RPAREN, LBRACE, RBRACE, SEMICOLON,
      COLON, COMMA, LSQB, RSQB,

      // Operators
      EQ, EQEQ, PLUS, FATARROW, STAR,

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
