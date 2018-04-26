import scala.collection._
import scala.ref.WeakReference
sealed trait Scope extends HasLoc {
  def getSymbol(name: String): Option[Symbol]
  def symbols: scala.collection.Map[String, ScopeEntry]
  def parent: WeakReference[Option[Scope]]
  def children: Iterable[WeakReference[Scope]]
}

case class ScopeBuilder(
  parent: WeakReference[Option[Scope]],
) extends Scope {
  private val _symbols: mutable.HashMap[String, ScopeEntry] = mutable.HashMap.empty
  private var _children: List[WeakReference[Scope]] = List()
  private var _loc: Loc = _

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

  def setSymbol(name: String, entry: ScopeEntry): Unit =
    _symbols += name -> entry

}

case class ScopeEntry(
  symbol: Symbol,
  typ: Type,
  loc: Loc
) extends HasLoc


