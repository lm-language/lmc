package lmc

import java.nio.file.{Path, Paths}

import lmc.common._

import scala.concurrent.Future
import scala.collection._
import lmc.types.{Primitive, Type, Kind}
import diagnostics._
import io.File
import lmc.syntax.{ Parsed, Typed }

class Compiler(paths: Iterable[Path]) {
  private val _typedSourceFiles = mutable.Map.empty[Path, Typed.SourceFile]
  private val _parsedSourceFiles = mutable.Map.empty[Path, Parsed.SourceFile]

  private var _id = 0
  var _nextUninferredId = 0
  val _symbolTypes = mutable.Map.empty[Symbol, Type]
  private val _symbolKinds = mutable.Map.empty[Symbol, Kind]


  private def makeUninferred(): Type = {
    val id = _nextUninferredId
    _nextUninferredId += 1
    types.UnInferred(id)
  }

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
    val scopeBuilder = ScopeBuilder(parent = None)
    scopeBuilder.setLoc(loc)
    for ((name, typ) <- entries) {
      scopeBuilder.setSymbol(name, ScopeEntry(loc, makeSymbol(name), typ))
    }
    val primitiveTypes = Map(
      "Unit" -> (Primitive.Unit, Kind.Star),
      "Int" -> (Primitive.Int, Kind.Star),
      "Bool" -> (Primitive.Bool, Kind.Star)
    )
    for ((name, (t, kind)) <- primitiveTypes) {
      val symbol = makeSymbol(name)
      scopeBuilder.setTypeSymbol(name, (symbol, t, kind))
      setKindOfSymbol(symbol, kind)
    }
    scopeBuilder
  }

  def compile(): Future[Unit] = {
    Future.unit
  }

  def getCheckedSourceFile(path: Path): Typed.SourceFile = {
    _typedSourceFiles.get(path) match {
      case Some(sf) =>
        sf
      case None =>
        val parsed = getParsedSourceFile(path)
        val checker = new TypeChecker(
          this,
          _setTypeOfSymbol = (symbol, typ) => {
            _symbolTypes.update(symbol, typ)
          },
          setKindOfSymbol,
          makeUninferred
        )
        val checkedSourceFile = checker.checkSourceFile(parsed)
        cacheCheckedSourceFile(path, checkedSourceFile)
        checkedSourceFile
    }
  }

  private def setKindOfSymbol(symbol: Symbol, kind: Kind): Unit = {
    _symbolKinds.update(symbol, kind)
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

  def getKind(symbol: Symbol): Option[Kind] = {
    _symbolKinds.get(symbol)
  }

  def makeSymbol(text: String): Symbol = {
    val id = _id
    _id += 1

    Symbol(id, text)
  }
}
