package lmc.syntax
import lmc.common.ScopeBuilder

object Parsed extends Syntax {
  override type Name = String
  override type _Scope = ScopeBuilder
  override type _Type = Unit
  override type _Kind = Unit
  override type _Parent = Unit
}
