package lmc

import lmc.common.{ Scope, Symbol }
import lmc.types.{Type, Kind}

trait Context {
  val PrimitiveScope: Scope
  def makeSymbol(text: String): Symbol
  def nextMetaId(): Int
}
object Context {
  trait TC extends Context {
    def setTypeOfSymbol(symbol: Symbol, typ: Type): Unit
    def getTypeOfSymbol(symbol: Symbol): Option[Type]
    def getTypeVar(symbol: Symbol): Option[Type]
    def setTypeVar(symbol: Symbol, typ: Type): Unit
    def setKindOfSymbol(symbol: Symbol, kind: Kind): Unit
    def getKindOfSymbol(symbol: Symbol): Option[Kind]
    def makeGenericType(name: String): Type

    def assignGeneric(n: Int, t: Type): Unit
    def getGeneric(n: Int): Option[Type]

    def getVars(): collection.Map[Symbol, Type]

  }
}

