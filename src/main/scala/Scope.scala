import scala.ref.WeakReference

case class Scope(
  loc: Loc,
  symbols: Map[String, ScopeEntry] = Map.empty,
  children: Iterable[Scope] = List(),
  parent: WeakReference[Option[Scope]] = new WeakReference(None)
) extends HasLoc

case class ScopeEntry(
  symbol: Symbol,
  typ: Type,
  loc: Loc
) extends HasLoc

