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
  case object ModifierOnInclude extends Variant
  case object KindExpected extends Variant
  case class UnexpectedChar(c: String) extends Variant
  case class InvalidOperator(c: String) extends Variant
  case class TokenExpected(c: String) extends Variant
  case class DuplicateBinding(name: String) extends Variant
  case class UnBoundVar(name: String) extends Variant
  case class UseBeforeAssignment(name: String) extends Variant
  case class TypeMismatch(expected: Type, found: Type) extends Variant
  case class TypeMismatchTrace(expected: Type, found: Type, trace: Iterable[Diagnostic]) extends Variant
  case class UnBoundTypeVar(name: String) extends Variant
  case class FnParamMismatch(expected: List[Type], found: List[Type]) extends Variant
  case class FuncParamLabelMismatch(name: String) extends Variant
  case class NotAFunction(foundTyp: Type) extends Variant
  case object ExtraParam extends Variant
  case object PositionalArgAfterLabelled extends Variant
  case object PositionalParamAfterLabelled extends Variant
  case object MissingTypeAnnotation extends Variant
  case object ExtraArg extends Variant
  case class DuplicateLabelArg(name: String) extends Variant
  case class NoSuchParamLabel(name: String) extends Variant
  case object CyclicType extends Variant
  case class KindMismatch(expected: lmc.types.Kind, found: lmc.types.Kind) extends Variant
  case object ExtraTypeArg extends Variant
  case object NotAModule extends Variant
  case class NoSuchTypeProperty(name: String) extends Variant
  case class NoSuchValueProperty(name: String) extends Variant
  case class DuplicateModifier(modifier: String) extends Variant
  case object MissingBodyInNonAbstract extends Variant
  case object UnexpectedBodyInAbstract extends Variant
  case object ComplexPatternInAbstract extends Variant
  case object MissingTypeAnnotationInAbstract extends Variant
  case class TriedToIncludeNonModule(typ: Type) extends Variant
  case class TriedToExtendNonAbstract(typ: Type) extends Variant
  case class ConflictingDecls(name: String) extends Variant
  case class InvalidTypeOverride(expectedKind: lmc.types.Kind, kind: lmc.types.Kind) extends Variant
  case class InvalidValueOverride(expectedType: lmc.types.Type, kind: lmc.types.Type) extends Variant
  case object NoSuchVariant extends Variant

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
    override def toString: String = message
  }
}
