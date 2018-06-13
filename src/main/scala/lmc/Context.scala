package lmc

import lmc.common.{Scope, Symbol}
import lmc.types.{Kind, Type}

trait Context {
  val PrimitiveScope: Scope
  def makeSymbol(text: String): Symbol
}

object Context {
  trait Parser extends Context {
    def nextMetaId: Int
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

