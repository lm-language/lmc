package lmc

import lmc.common.{HasLoc, Loc}
import lmc.types.Type

object diagnostics {
  sealed trait Variant
  case class ExpectedToken(variant: syntax.token.Variant) extends Variant
  case class DeclarationExpected() extends Variant
  case class ExpressionExpected() extends Variant
  case class PatternExpected() extends Variant
  case class TypeExpected() extends Variant
  case class UnexpectedChar(c: String) extends Variant
  case class InvalidOperator(c: String) extends Variant
  case class TokenExpected(c: String) extends Variant
  case class DuplicateBinding(name: String) extends Variant
  case class UnBoundVar(name: String) extends Variant
  case class UseBeforeAssignment(name: String) extends Variant
  case class TypeMismatch(expected: Type, found: Type) extends Variant
  case class UnBoundTypeVar(name: String) extends Variant
  case class FnParamMismatch(expected: List[Type], found: List[Type]) extends Variant
  case class NotAFunction(foundTyp: Type) extends Variant
  case object ExtraParam extends Variant
  case object PositionalArgAfterLabelled extends Variant
  case object PositionalParamAfterLabelled extends Variant
  case class FuncParamLabelMismatch(expected: String) extends Variant
  case object MissingTypeAnnotation extends Variant
  case class MissingArguments(params: Iterable[(Option[String], Type)]) extends Variant {
    override def toString: String = {
      val paramsStr = params.foldLeft("")(
        (prev, current) => s"$prev,${
          current match {
            case (Some(label), typ) =>
              s"$label:$typ"
            case (None, typ) => typ
          }
        }"
      )
        .drop(1)
      s"MissingArguments($paramsStr)"
    }
  }

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
