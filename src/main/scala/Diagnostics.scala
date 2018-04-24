
object Diagnostics {
  sealed trait Variant
  case class ExpectedToken(variant: Tokens.Variant) extends Variant
  case class DeclarationExpected() extends Variant
  case class ExpressionExpected() extends Variant
  case class PatternExpected() extends Variant
  case class UnexpectedChar(c: String) extends Variant
  case class InvalidOperator(c: String) extends Variant
  case class TokenExpected(c: String) extends Variant
  case class DuplicateBinding(name: String) extends Variant
  case class UnBoundVar(name: String) extends Variant
  case class UseBeforeAssignment(name: String) extends Variant

  object Severity extends Enumeration {
    type T = Value
    val Warning, Error = Value
  }
  type Severity = Severity.T

  final case class Diagnostic(
    variant: Variant,
    severity: Severity.T,
    loc: Loc
  ) extends HasLoc {
    def message: String = variant.toString
  }
}