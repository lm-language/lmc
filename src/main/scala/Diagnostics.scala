import Loc._

object Diagnostics {
  sealed trait Variant
  case class ExpectedToken(variant: Tokens.Variant) extends Variant
  case class DeclarationExpected() extends Variant

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