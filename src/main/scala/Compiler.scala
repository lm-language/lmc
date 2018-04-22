import java.nio.file.Path
import Diagnostics.Diagnostic
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import Syntax._
import scala.collection._
import IO._

object Compiler {
  final class Compiler(paths: Iterable[Path])(implicit ec: ExecutionContext) {
    private var _errors = List()
    private val _typedSourceFiles = mutable.Map.empty[Path, Typed.SourceFile]
    private val _parsedSourceFiles = mutable.Map.empty[Path, Parsed.SourceFile]
    def errors: Iterable[Diagnostic] = _errors

    def compile(): Future[Unit] = {
      Future.unit
    }

    def getCheckedSourceFile(path: Path): Typed.SourceFile = {
      if (true) {
        val loc = Loc(path, Pos(1, 1), Pos(1, 2))
        return Typed.SourceFile(
          meta = Meta(
            loc
          ),
          declarations = List(),
          scope = Scope(loc, Map.empty)
        )
      }
      _typedSourceFiles.get(path) match {
        case Some(sf) => sf
        case None =>
          val parsed = getParsedSourceFile(path)
          ???
      }
    }

    def getParsedSourceFile(path: Path): Parsed.SourceFile = {
      val chars = File(path)
      val tokens = Lexer(path, chars)
      val parser = Parser(path, tokens)
      val sourceFile = parser.parseSourceFile()
      cacheParsedSourceFile(path, sourceFile)
      sourceFile
    }

    private def cacheParsedSourceFile(path: Path, sourceFile: Parsed.SourceFile) = {
      _parsedSourceFiles.put(path, sourceFile)
    }
  }
}