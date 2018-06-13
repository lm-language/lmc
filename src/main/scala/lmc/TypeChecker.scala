package lmc


final class TypeChecker(
  private val ctx: Context.TC
) {
  import lmc.syntax.{Parsed => P, Typed => T}

  private val Primitive = ctx.Primitive

  def inferSourceFile(sourceFile: P.SourceFile): T.SourceFile = {
    ???
  }
}
