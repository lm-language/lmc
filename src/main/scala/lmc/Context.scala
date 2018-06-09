package lmc

import lmc.common.{Scope, Symbol}
import lmc.types.{Kind, Type}

trait Context {
  val PrimitiveScope: Scope
  def makeSymbol(text: String): Symbol
  def nextMetaId(): Int
  def getTypedExpr(e: syntax.Named.Expr): syntax.Typed.Expr
  def getDeclOfSymbol(name: Symbol): Option[syntax.Named.Declaration]
  def getSubstSymbol(sym: Symbol): Symbol
  def getDeclOfTypeSymbol(sym: Symbol): Option[syntax.Named.Declaration]
  def getEnumVariants(sym: Symbol): Vector[Symbol]

  def getNamedSourceFile(id: Int): Option[syntax.Named.SourceFile]
  def getNamedDecl(id: Int): Option[syntax.Named.Declaration]
  def getNamedExpr(id: Int): Option[syntax.Named.Expr]
  def getNamedPattern(id: Int): Option[syntax.Named.Pattern]
  def getNamedIdent(id: Int): Option[syntax.Named.Ident]

  def getParsedNode(id: Int): Option[syntax.Parsed.Node]

  def getParsedParentOf(id: Int): Option[syntax.Parsed.Node]
}

object Context {
  trait Parser extends Context {
    def  setParsedNode(id: Int, node: syntax.Parsed.Node): Unit
  }
  trait Renamer extends Context {
    def setDeclOfSymbol(name: Symbol, decl: syntax.Named.Declaration): Unit
    def setDeclOfTypeSymbol(name: Symbol, decl: syntax.Named.Declaration): Unit
    def setEnumVariants(name: Symbol, variants: Iterable[Symbol]): Unit
    def setNamedSourceFile(id: Int, sourceFile: syntax.Named.SourceFile): Unit
    def setNamedDecl(id: Int, decl: syntax.Named.Declaration): Unit
    def setNamedExpr(id: Int, expr: syntax.Named.Expr): Unit
    def setNamedPattern(id: Int, pattern: syntax.Named.Pattern): Unit
    def setNamedIdent(id: Int, ident: syntax.Named.Ident): Unit
    def setParsedParentOf(node: syntax.Parsed.Node, parentNode: syntax.Parsed.Node): Unit
  }

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

    val Primitive: Primitive

  }
}

abstract class Primitive {
  val Int: Type
  val Unit: Type
  val Bool: Type
}

