package lmc

import lmc.common.{ Scope, Symbol }
import lmc.types.Type

trait Context {
  val PrimitiveScope: Scope
  def makeSymbol(text: String): Symbol
}
object Context {
  trait TC extends Context {
    def setTypeOfSymbol(symbol: Symbol, typ: Type): Unit
    def getTypeOfSymbol(symbol: Symbol): Option[Type]
    def getTypeVar(symbol: Symbol): Option[Type]
  }
}

