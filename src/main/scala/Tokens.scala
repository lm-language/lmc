object Tokens {
  object Variant extends Enumeration {
    type Variant = Value
    val
      ID, INT, EOF,
      // keywords
      LET, IF, ELSE,

      // Punctuation
      VBAR, LPAREN, RPAREN, LBRACE, RBRACE,

      // Operators
      EQ, EQEQ, PLUS,

      UNEXPECTED_CHAR,
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
