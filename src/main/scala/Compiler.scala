import java.nio.file.{Path, Paths}

import Diagnostics.Diagnostic

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import Syntax._

import scala.collection._
import IO._

import scala.ref.WeakReference

final class Compiler(paths: Iterable[Path])(implicit ec: ExecutionContext) {
  private val _typedSourceFiles = mutable.Map.empty[Path, Typed.SourceFile]
  private val _parsedSourceFiles = mutable.Map.empty[Path, Parsed.SourceFile]

  private val _symbolTypes = mutable.Map.empty[Symbol, Type]


  val PrimitivesScope: Scope = {
    val loc = Loc(
      path = Paths.get("<builtin>"),
      start = Pos(0, 0),
      end = Pos(0, 0))
    val entries = Map(
      "unit" -> Primitive.Unit,
      "true" -> Primitive.Bool,
      "false" -> Primitive.Bool
    )
    val scopeBuilder = ScopeBuilder(parent = WeakReference(None))
    scopeBuilder.setLoc(loc)
    for ((name, typ) <- entries) {
      scopeBuilder.setSymbol(name, ScopeEntry(loc, makeSymbol(name), typ))
    }
    scopeBuilder
  }

  def compile(): Future[Unit] = {
    Future.unit
  }

  def getCheckedSourceFile(path: Path): Typed.SourceFile = {
    _typedSourceFiles.get(path) match {
      case Some(sf) => sf
      case None =>
        val parsed = getParsedSourceFile(path)
        val checker = new TypeChecker(this, (symbol, typ) => {
          _symbolTypes.update(symbol, typ)
        })
        val checkedSourceFile = checker.checkSourceFile(parsed)
        cacheCheckedSourceFile(path, checkedSourceFile)
        checkedSourceFile
    }
  }

  def getSourceFileScope(path: Path): Scope = {
    getCheckedSourceFile(path).scope
  }

  def getSourceFileDiagnostics(path: Path): Iterable[Diagnostic] = {
    getCheckedSourceFile(path).errors.toSet union getParsedSourceFile(path).errors.toSet
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

  private def cacheCheckedSourceFile(path: Path, sourceFile: Typed.SourceFile) = {
    _typedSourceFiles.put(path, sourceFile)
  }

  def getType(symbol: Symbol): Option[Type] = {
    _symbolTypes.get(symbol)
  }

  private var _id = 0
  def makeSymbol(text: String): Symbol = {
    val id = _id
    _id += 1
    Symbol(id, text)
  }
}
