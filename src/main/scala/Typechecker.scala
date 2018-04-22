import Syntax._
import scala.collection.mutable
import scala.ref.WeakReference

object Typechecker {
  def apply(compiler: Compiler): Typechecker = new Typechecker(compiler)
}

final class Typechecker(compiler: Compiler) {
  case class MutableScope(
    var loc: Loc,
    var symbols: mutable.Map[String, ScopeEntry] = mutable.Map.empty,
    children: mutable.ArrayBuffer[Scope] = mutable.ArrayBuffer.empty,
  ) {
    def toScope: Scope = {
      Scope(
        loc = loc,
        symbols = symbols.toMap,
        children = this.children
      )
    }
  }
  private var scopeMaps = List.empty[MutableScope]

  def checkSourceFile(parsed: Parsed.SourceFile): Typed.SourceFile = {
    val (declarations, scope) = checkModule(parsed.loc, parsed.declarations)
    Typed.SourceFile(
      parsed.meta,
      declarations = declarations,
      scope = scope
    )
  }

  def checkModule(loc: Loc, declarations: Iterable[Parsed.Declaration]): (Iterable[Typed.Declaration], Scope) = {
    pushScope(loc)
    val scope = popScope()
    // TODO: Implement this
    (List(), scope)
  }

  private def pushScope(loc: Loc): Unit = {
    scopeMaps = MutableScope(loc)::scopeMaps
  }

  private def popScope(): Scope = {
    scopeMaps match {
      case Nil => throw new Exception("CompilerBug: Tried to pop scope from empty list")
      case mutableScope::tail =>
        scopeMaps = tail
        val scope = mutableScope.toScope
        tail match {
          case parent::_ =>
            parent.children += scope
          case  _ => ()
        }
        scope

    }
  }
}