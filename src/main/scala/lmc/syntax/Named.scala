package lmc.syntax
import lmc.common.{ScopeBuilder, Symbol}

object Named extends Syntax {
  type Name = Symbol
  type _Scope = ScopeBuilder
  type _Type = Unit
  type _Kind = Unit
  // id of parent node
  override type _Parent = Option[Int]
}

