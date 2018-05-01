package lmc.common

import java.nio.file.Paths
import scala.collection._
import scala.ref.WeakReference

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
  val empty: Scope = new Scope {
    override def parent: Option[WeakReference[Scope]] = None

    override val symbols: Map[String, ScopeEntry] = Map()
    override val typeSymbols: Map[String, TypeEntry] = Map()

    override val children: Iterable[WeakReference[Scope]] = List()

    override def getEntry(name: String): Option[ScopeEntry] = None

    override lazy val loc: Loc = Loc(Paths.get(""), Pos(0, 0), Pos(0, 0))
    override def typed: Scope = this
  }
}

case class ScopeBuilder(
  parent: Option[WeakReference[Scope]],
) extends Scope {
  private val _symbols: mutable.HashMap[String, ScopeEntry] = mutable.HashMap.empty
  private val _typeSymbols: mutable.HashMap[String, TypeEntry] = mutable.HashMap.empty
  private var _children: List[WeakReference[Scope]] = List()
  private var _loc: Loc = _

  override def loc: Loc = _loc

  def setLoc(loc: Loc): Unit =
    _loc = loc

  override def typeSymbols: Map[String, TypeEntry] = _typeSymbols

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

