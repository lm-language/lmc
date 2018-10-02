package lmc

import lmc.Value.Type
import lmc.common.Symbol

case class Env(
  types: Map[Symbol, Type],
  values: Map[Symbol, Value]
) {
  def getTypeOf(symbol: Symbol): Option[Type] = {
    types.get(symbol)
  }

  def getValueOf(symbol: Symbol): Option[Value] = {
    values.get(symbol)
  }

  def withType(symbol: Symbol, typ: Type): Env = {
    this.copy(
      types = this.types + (symbol -> typ)
    )
  }

  def withValue(symbol: Symbol, value: Value): Env = {
    this.copy(
      values = this.values + (symbol -> value)
    )
  }
}
