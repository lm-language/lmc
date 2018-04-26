package lmc.syntax
import lmc.common.ScopeBuilder

object Parsed extends Syntax {
  type Name = String
  type _Scope = ScopeBuilder
  type _Type = Unit
}
