package lmc.common

import java.nio.file.Paths
import scala.collection._
import scala.ref.WeakReference

import lmc.types._

sealed trait Scope extends HasLoc {
  def getSymbol(name: String): Option[Symbol] = {
    getEntry(name) map (_.symbol)
  }
  def getEntry(name: String): Option[ScopeEntry] = {
    symbols.get(name)
  }
  def symbols: scala.collection.Map[String, ScopeEntry]
  def typeSymbols: scala.collection.Map[String, TypeEntry]
  def parent: Option[WeakReference[Scope]]
  def children: Iterable[WeakReference[Scope]]

  def declMap: collection.Map[Symbol, Int]

  def typeMap: collection.Map[String, (Type, Symbol)]

  def resolveTypeEntry(name: String): Option[TypeEntry] = {
    typeSymbols.get(name) match {
      case Some(n) => Some(n)
      case None =>
        for {
          parentRef <- parent
          parentScope <- parentRef.get
          symbol <- parentScope.resolveTypeEntry(name)
        } yield symbol
    }
  }


  def resolve(name: String): Option[(Type, Symbol)] =
    typeMap.get(name) match {
      case Some(result) => Some(result)
      case None => parent.flatMap(_.get).flatMap(_.resolve(name))
    }

  def resolveEntry(name: String): Option[ScopeEntry] = {
    getEntry(name) match {
      case Some(n) => Some(n)
      case None =>
        for {
          parentRef <- parent
          parentScope <- parentRef.get
          symbol <- parentScope.resolveEntry(name)
        } yield symbol
    }
  }

  def resolveSymbol(name: String): Option[Symbol] = {
    resolveEntry(name) map (_.symbol)
  }

  def typed: Scope
}

object Scope {
  val empty: Scope = {
    val scopeBuilder = ScopeBuilder(parent = None)
    val loc = Loc(
      path = Paths.get("<builtin>"),
      start = Pos(0, 0),
      end = Pos(0, 0))
    scopeBuilder.setLoc(loc)
    scopeBuilder
  }
}

case class ScopeBuilder(
  parent: Option[WeakReference[Scope]],
) extends Scope {
  private val _symbols: mutable.HashMap[String, ScopeEntry] = mutable.HashMap.empty
  private val _typeSymbols: mutable.HashMap[String, TypeEntry] = mutable.HashMap.empty
  private var _children: List[WeakReference[Scope]] = List()
  private var _loc: Loc = _

  private val _typeMap: mutable.HashMap[String, (Type, Symbol)] = mutable.HashMap.empty

  private val _declMap = mutable.HashMap.empty[Symbol, Int]

  override def loc: Loc = _loc

  def setLoc(loc: Loc): Unit =
    _loc = loc

  override def typeSymbols: Map[String, TypeEntry] = _typeSymbols

  override def typeMap: Map[String, (Type, Symbol)] =
    _typeMap

  def setType(name: String, symbol: Symbol, typ: Type) =
    _typeMap.update(name, (typ, symbol))

  override def declMap: collection.Map[Symbol, Int] =
    _declMap

  def addDeclaration(symbol: Symbol, nodeId: Int): Unit = {
    _declMap.put(symbol, nodeId)
  }

  override def children: Iterable[WeakReference[Scope]] = _children

  def symbols: Map[String, ScopeEntry] = _symbols

  def addChild(scope: Scope): Unit = {
    _children = WeakReference(scope)::_children
  }

  def setSymbol(name: String, entry: ScopeEntry): Unit =
    _symbols += name -> entry

  def setTypeVar(name: String, entry: TypeEntry): Unit = {
    _typeSymbols.put(name, entry)
  }

  override def typed: Scope = this
}

case class TypeEntry(
  symbol: Symbol
)

case class ScopeEntry(
  symbol: Symbol,
  validAfter: Option[Pos] = None
) {
  override def toString: String =
    s"""<$symbol:${symbol.id}>"""
}

