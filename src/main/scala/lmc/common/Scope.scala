package lmc.common

import java.nio.file.Paths
import scala.collection._
import scala.ref.WeakReference

sealed trait Scope extends HasLoc {
  def symbols: scala.collection.Map[String, Symbol]
  def parent: Option[WeakReference[Scope]]
  def children: Iterable[WeakReference[Scope]]

  def resolve(name: String): Option[Symbol] =
    symbols.get(name) match {
      case Some(result) => Some(result)
      case None => parent.flatMap(_.get).flatMap(_.resolve(name))
    }

  def getSymbol(name: String): Option[Symbol] = {
    symbols.get(name)
  }
}

object Scope {
  val empty: Scope = {
    val scopeBuilder = ScopeBuilder(parent = None)
    val loc = Loc(
      path = Paths.get("builtin"),
      start = Pos(0, 0),
      end = Pos(0, 0))
    scopeBuilder.setLoc(loc)
    scopeBuilder
  }
}

case class ScopeBuilder(
  parent: Option[WeakReference[Scope]],
  private val _symbols: mutable.HashMap[String, Symbol] = mutable.HashMap.empty
) extends Scope {
  private var _children: List[WeakReference[Scope]] = List()
  private var _loc: Loc = _

  override def loc: Loc = _loc

  def setLoc(loc: Loc): Unit =
    _loc = loc

  override def children: Iterable[WeakReference[Scope]] = _children

  def symbols: Map[String, Symbol] = _symbols

  def addChild(scope: Scope): Unit = {
    _children = WeakReference(scope)::_children
  }

  def setSymbol(name: String, symbol: Symbol): Unit =
    _symbols += name -> symbol
}

