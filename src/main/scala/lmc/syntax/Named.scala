package lmc.syntax
import lmc.common.{ Scope, Symbol }

object Named extends Syntax {
  type Name = Symbol
  type _Scope = Scope
  type _Type = Unit
  type _Kind = Unit
}

