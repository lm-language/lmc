import scala.collection._
import scala.ref.WeakReference
sealed trait Scope extends HasLoc {
  def getSymbol(name: String): Option[Symbol]
  def parent: WeakReference[Option[Scope]]
}

case class ScopeBuilder(
  parent: WeakReference[Option[Scope]]
) extends Scope {
  private val symbols: mutable.HashMap[String, ScopeEntry] = mutable.HashMap.empty

  private var _loc: Loc = _

  override def loc: Loc = _loc

  def setLoc(loc: Loc): Unit =
    _loc = loc

  override def getSymbol(name: String): Option[Symbol] =
    symbols get name map (_.symbol)

  def setSymbol(name: String, entry: ScopeEntry): Unit =
    symbols += name -> entry
}

case class ScopeEntry(
  symbol: Symbol,
  typ: Type,
  loc: Loc
) extends HasLoc


