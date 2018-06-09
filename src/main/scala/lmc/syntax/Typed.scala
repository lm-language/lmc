package lmc.syntax

import lmc.common.{Scope, Symbol}
import lmc.types.{Kind, Type}

object Typed extends Syntax {
  type Name = Symbol
  type _Scope = Scope
  type _Type = Type
  type _Kind = Kind
  override type _Parent = Option[Int]
}
