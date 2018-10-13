package lmc


import lmc.common.{Scope, Symbol}
import lmc.syntax.Parsed
import lmc.Value.Type
import lmc.diagnostics.Diagnostic


trait Context {
  def makeSymbol(text: String, declaration: syntax.Parsed.Declaration): Symbol
  def makeErrorSymbol(text: String): Symbol
  def getParsedNode(id: Int): Option[Parsed.Node]
  val Primitive: Primitive

  def getTypeOfSymbol(symbol: Symbol): Type

  def addError(diagnostic: Diagnostic): Unit
}

object Context {
  trait Parser extends Context {
    def nextMetaId: Int
    def setParsedNode(id: Int, node: Parsed.Node): Unit
    def PreludeScope: Scope
  }

  trait Binder extends Context {
    def setDeclOf(symbol: Symbol, decl: Parsed.Declaration): Unit
  }

  trait TC extends Context {
    def setType(symbol: Symbol, typ: Type): Unit
  }

}

abstract class Primitive {
  val Int: Type
  val Unit: Type
  val Bool: Type
  val Type: Type

  val IntSymbol: Symbol
  val UnitSymbol: Symbol
  val BoolSymbol: Symbol
  val TypeSymbol: Symbol

  val True: Value
  val False: Value
}

