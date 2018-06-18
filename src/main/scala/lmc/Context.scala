package lmc

import lmc.common.{Scope, Symbol}
import lmc.types.{Type}
import lmc.syntax.Parsed

trait Context {
  val PrimitiveScope: Scope
  def makeSymbol(text: String): Symbol
  def getParsedNode(id: Int): Option[Parsed.Node]
  def getDeclOf(symbol: Symbol): Option[Parsed.Declaration]
}

object Context {
  trait Parser extends Context {
    def nextMetaId: Int

    def setParsedNode(id: Int, node: Parsed.Node): Unit
  }
  trait Binder extends Context {
    def setDeclOf(symbol: Symbol, decl: Parsed.Declaration): Unit
  }
  trait Renamer extends Context {}

  trait TC extends Context {
    val Primitive: Primitive
    def makeGenericType(text: String): Type
  }
}

abstract class Primitive {
  val Int: Type
  val Unit: Type
  val Bool: Type
}

