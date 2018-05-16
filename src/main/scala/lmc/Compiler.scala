package lmc

import java.nio.file.{Path, Paths}

import lmc.common._

import scala.concurrent.Future
import scala.collection._
import lmc.types.{ExistentialInstance, Kind, Type}
import diagnostics._
import io.File
import lmc.syntax.{Parsed, Typed}

class Compiler(paths: Iterable[Path], debug: (String) => Unit = ((_) => {})) extends Context with Context.TC {
  private val _typedSourceFiles = mutable.Map.empty[Path, Typed.SourceFile]
  private val _parsedSourceFiles = mutable.Map.empty[Path, Parsed.SourceFile]

  private var _id = 0
  val _symbolTypes = mutable.Map.empty[Symbol, Type]
  private val _symbolKinds = mutable.Map.empty[Symbol, Kind]
  private val _typeVariables = mutable.Map.empty[Symbol, Type]
  private var _nextGenericId = 0
  private val _generics = mutable.Map.empty[Int, Type]
  private var _nextNodeId = 0

  override def getVars(): collection.Map[Symbol, Type] = _symbolTypes


  private val primitiveTypes = Map(
    "Unit" -> Kind.Star,
    "Int" -> Kind.Star,
    "Bool" -> Kind.Star
  ).map((tuple) => {
    val symbol = makeSymbol(tuple._1)
    (tuple._1, (lmc.types.Constructor(symbol, tuple._2), tuple._2))
  })

  private def primitive(str: String): Type = {
    primitiveTypes.get(str).map(_._1).get
  }
  override val Primitive: Primitive = new Primitive {
    override val Int: Type = primitive("Int")
    override val Bool: Type = primitive("Bool")
    override val Unit: Type = primitive("Unit")
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

    val scopeBuilder = ScopeBuilder(parent = None)
    scopeBuilder.setLoc(loc)
    for ((name, typ) <- entries) {
      val symbol = makeSymbol(name)
      scopeBuilder.setSymbol(name, ScopeEntry(symbol, None))
      setTypeOfSymbol(symbol, typ)
    }
    for ((name, (t, kind)) <- primitiveTypes) {
      val symbol = t.symbol
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

  override def nextMetaId(): Int = {
    val id = _nextNodeId
    _nextNodeId += 1
    id
  }

  def getCheckedSourceFile(path: Path): Typed.SourceFile = {
    _typedSourceFiles.get(path) match {
      case Some(sf) =>
        sf
      case None =>
        val parsed = getParsedSourceFile(path)
        val checker = new TypeChecker(this)
        val checkedSourceFile = checker.inferSourceFile(parsed)
        cacheCheckedSourceFile(path, checkedSourceFile)
        checkedSourceFile
    }
  }

  override def setKindOfSymbol(symbol: Symbol, kind: Kind): Unit = {
    _symbolKinds.update(symbol, kind)
  }

  override def getKindOfSymbol(symbol: Symbol): Option[Kind] = {
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

  def getSourceFileDiagnostics(
    path: Path, refresh: Boolean = false
  ): Iterable[Diagnostic] = {
    if (refresh) {
      this._parsedSourceFiles.remove(path)
      this._typedSourceFiles.remove(path)
    }
    val result = getCheckedSourceFile(path)
      .errors.toSet union getParsedSourceFile(path).errors.toSet

    result
  }

  def getParsedSourceFile(path: Path): Parsed.SourceFile = {
    _parsedSourceFiles.get(path) match {
      case Some(p) =>
        p
      case None =>
        val chars = File(path)
        val tokens = Lexer(path, chars)
        val parser = new Parser(this, path, tokens)
        val sourceFile = parser.parseSourceFile()
        cacheParsedSourceFile(path, sourceFile)
        sourceFile
    }

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

  override def assignGeneric(n: Int, t: Type): Unit =
    _generics.put(n, t)

  override def getGeneric(n: Int): Option[Type] =
    _generics.get(n)

  override def makeSymbol(text: String): Symbol = {
    val id = _id
    _id += 1

    Symbol(id, text)
  }

  override def makeGenericType(name: String): Type = {
    val id = _nextGenericId
    _nextGenericId += 1
    ExistentialInstance(id, name)
  }

  def getHoverInfo(file: Path, pos: Pos): String = {
    // TODO: Implement hover functionality
    return null
  }
}
