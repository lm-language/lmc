import Syntax._

object Typechecker {
  def apply(compiler: Compiler): Typechecker = new Typechecker(compiler)
}

final class Typechecker(compiler: Compiler) {
  def checkSourceFile(parsed: Parsed.SourceFile): Typed.SourceFile =
    Typed.SourceFile(
      meta = parsed.meta,
      declarations = List(),
      scope = Scope(parsed.loc)
    )
}