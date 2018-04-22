import Loc._
object Tokens {
  object Variant extends Enumeration {
    type Variant = Value
    val ID, INT, EOF = Value
  }
  type Variant = Variant.Variant
  final case class Token(
    variant: Variant,
    loc: Loc,
    lexeme: String
  ) extends HasLoc
}
