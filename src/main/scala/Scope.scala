import java.nio.file.{Path, Paths}

import scala.collection._
import scala.ref.WeakReference
sealed trait Scope extends HasLoc {
  type TypeEntry = (Symbol, Kind)
  def getSymbol(name: String): Option[Symbol]
  def symbols: scala.collection.Map[String, ScopeEntry]
  def typeSymbols: scala.collection.Map[String, TypeEntry]
  def parent: WeakReference[Option[Scope]]
  def children: Iterable[WeakReference[Scope]]
  def getEntry(name: String): Option[ScopeEntry]
}

object Scope {
  val empty: Scope = new Scope {override def parent: WeakReference[Option[Scope]] = WeakReference(None)

    override val symbols: Map[String, ScopeEntry] = Map()

    override val children: Iterable[WeakReference[Scope]] = List()

    override val typeSymbols: collection.Map[String, TypeEntry] = Map.empty

    override def getEntry(name: String): Option[ScopeEntry] = None

    override def getSymbol(name: String): Option[Symbol] = None

    override lazy val loc: Loc = Loc(Paths.get(""), Pos(0, 0), Pos(0, 0))
  }
}

case class ScopeBuilder(
  parent: WeakReference[Option[Scope]],
) extends Scope {
  private val _symbols: mutable.HashMap[String, ScopeEntry] = mutable.HashMap.empty
  private var _children: List[WeakReference[Scope]] = List()
  private var _loc: Loc = _
  private val _typeSymbols: mutable.HashMap[String, TypeEntry] = mutable.HashMap.empty

  override def loc: Loc = _loc

  def setLoc(loc: Loc): Unit =
    _loc = loc

  override def children: Iterable[WeakReference[Scope]] = _children

  def symbols: Map[String, ScopeEntry] = _symbols

  def addChild(scope: Scope): Unit = {
    _children = WeakReference(scope)::_children
  }

  override def getSymbol(name: String): Option[Symbol] =
    _symbols get name map (_.symbol)

  override def getEntry(name: String): Option[ScopeEntry] =
    _symbols get name

  def setSymbol(name: String, entry: ScopeEntry): Unit =
    _symbols += name -> entry


  override def typeSymbols: Map[String, TypeEntry] = _typeSymbols
}

case class ScopeEntry(
  loc: Loc,
  symbol: Symbol,
  typ: Type,
) extends HasLoc


