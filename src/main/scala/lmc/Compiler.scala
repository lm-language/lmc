package lmc

import java.nio.file.{Path, Paths}

import lmc.common._

import scala.concurrent.Future
import scala.collection._
import lmc.types.{Generic, Kind, Primitive, Type}
import diagnostics._
import io.File
import lmc.syntax.{Parsed, Typed}

class Compiler(paths: Iterable[Path]) extends Context with Context.TC {
  private val _typedSourceFiles = mutable.Map.empty[Path, Typed.SourceFile]
  private val _parsedSourceFiles = mutable.Map.empty[Path, Parsed.SourceFile]

  private var _id = 0
  var _nextUninferredId = 0
  val _symbolTypes = mutable.Map.empty[Symbol, Type]
  private val _symbolKinds = mutable.Map.empty[Symbol, Kind]
  private val _typeVariables = mutable.Map.empty[Symbol, Type]
  private val _nextGenericId = 0


  private def makeUninferred(): Type = {
    val id = _nextUninferredId
    _nextUninferredId += 1
    types.UnInferred(id)
  }

  override val PrimitiveScope: Scope = {
    val loc = Loc(
      path = Paths.get("<builtin>"),
      start = Pos(0, 0),
      end = Pos(0, 0))
    val entries = Map(
      "unit" -> Primitive.Unit,
      "true" -> Primitive.Bool,
      "false" -> Primitive.Bool
    )
    val primitiveTypes = Map(
      "Unit" -> (Primitive.Unit, Kind.Star),
      "Int" -> (Primitive.Int, Kind.Star),
      "Bool" -> (Primitive.Bool, Kind.Star)
    )

    val scopeBuilder = ScopeBuilder(parent = None)
    scopeBuilder.setLoc(loc)
    for ((name, typ) <- entries) {
      val symbol = makeSymbol(name)
      scopeBuilder.setSymbol(name, ScopeEntry(symbol, None))
      setTypeOfSymbol(symbol, typ)
    }
    for ((name, (t, kind)) <- primitiveTypes) {
      val symbol = makeSymbol(name)
      _typeVariables += symbol -> t
      scopeBuilder.setTypeVar(name, TypeEntry(symbol))
      setKindOfSymbol(symbol, kind)
      setTypeVar(symbol, t)
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
        val checker = new TypeChecker(this)
        val checkedSourceFile = checker.checkSourceFile(parsed)
        cacheCheckedSourceFile(path, checkedSourceFile)
        checkedSourceFile
    }
  }

  override def setKindOfSymbol(symbol: Symbol, kind: Kind): Unit = {
    _symbolKinds.update(symbol, kind)
  }

  def getKindOfSymbol(symbol: Symbol): Option[Kind] = {
    _symbolKinds.get(symbol)
  }


  override def setTypeVar(symbol: Symbol, typ: Type): Unit = {
    _typeVariables.update(symbol, typ)
  }

  override def getTypeVar(symbol: Symbol): Option[Type] =
    _typeVariables.get(symbol)

  override def setTypeOfSymbol(symbol: Symbol, typ: Type): Unit = {
    _symbolTypes.update(symbol, typ)
  }

  def getTypeOfSymbol(symbol: Symbol): Option[Type] = {
    _symbolTypes.get(symbol)
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
    val parser = new Parser(this, path, tokens)
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

  override def makeSymbol(text: String): Symbol = {
    val id = _id
    _id += 1

    Symbol(id, text)
  }

  override def makeGenericType(name: String): Type = {
    val id = _nextGenericId
    _nextUninferredId += 1
    Generic(id, name)
  }
}
