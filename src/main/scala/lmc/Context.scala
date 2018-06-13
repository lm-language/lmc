package lmc

import lmc.common.{Scope, Symbol}
import lmc.types.{Type}
import lmc.syntax.Parsed

trait Context {
  val PrimitiveScope: Scope
  def makeSymbol(text: String): Symbol
  def getParsedNode(id: Int): Option[Parsed.Node]
}

object Context {
  trait Parser extends Context {
    def nextMetaId: Int

    def setParsedNode(id: Int, node: Parsed.Node): Unit
  }
  trait Binder extends Context {}
  trait Renamer extends Context {}

  trait TC extends Context {
    val Primitive: Primitive
  }
}

abstract class Primitive {
  val Int: Type
  val Unit: Type
  val Bool: Type
}

