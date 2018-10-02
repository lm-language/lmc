package lmc.syntax

import lmc.common.{Scope, Symbol}
import lmc.Value.Type

object Typed extends Syntax {
  type Name = Symbol
  type _Scope = Scope
  type _Type = Type
}
