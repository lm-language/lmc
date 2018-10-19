package lmc

import java.nio.file.Path

import lmc.common._

import scala.concurrent.Future
import scala.collection.mutable
import diagnostics._
import io.{File, Stream, StringStream}
import lmc.syntax.{Parsed, Typed}
import lmc.Value.{Type, Constructor}

import scala.collection.mutable.ListBuffer

class Compiler(paths: Iterable[Path], debug: String => Unit = _ => {})
  extends Context
    with Context.TC
    with Context.Parser
    with Context.Binder {
  private val _charStreams = mutable.Map.empty[Path, Stream[Char]]
  private val _typedSourceFiles = mutable.WeakHashMap.empty[Path, Typed.SourceFile]
  private val _parsedSourceFiles = mutable.WeakHashMap.empty[Path, Parsed.SourceFile]

  private var _id = 0
  private var _nextGenericId = 0
  private var _nextNodeId = 0
  private val _parsedNodes = mutable.WeakHashMap.empty[Int, Parsed.Node]
  private val _declarationOf = mutable.HashMap.empty[Symbol, Int]

  private val _types = mutable.HashMap.empty[Symbol, Type]

  private val builtinDeclaration = Parsed.Declaration.Error(Parsed.ImmutableMeta(
    -1, null, null, null, ()
  ))

  private val _errorDeclaration = Parsed.Declaration.Error(Parsed.ImmutableMeta(
    -1, null, null, null, ()
  ))

  private val _sourceFileDiagnostics = mutable.HashMap.empty[Path, ListBuffer[Diagnostic]]


  override val Primitive: Primitive = new Primitive {
    override val BoolSymbol: Symbol = makeSymbol("Bool", builtinDeclaration)
    override val IntSymbol: Symbol = makeSymbol("Int", builtinDeclaration)
    override val UnitSymbol: Symbol = makeSymbol("Unit", builtinDeclaration)
    override val TypeSymbol: Symbol = makeSymbol("Type", builtinDeclaration)

    override val Int: Type = makePrimitive(IntSymbol)
    override val Unit: Type = makePrimitive(UnitSymbol)
    override val Bool: Type = makePrimitive(BoolSymbol)
    override val Type: Type = makePrimitive(TypeSymbol)

    override val True: Type = Value.Constructor(_trueSymbol)
    override val False: Type = Value.Constructor(_falseSymbol)
  }

  private val _boolEqSymbol = makeSymbol("boolEq", builtinDeclaration)
  private val _trueSymbol = makeSymbol("true", builtinDeclaration)
  private val _falseSymbol = makeSymbol("false", builtinDeclaration)
  private val _unitSymbol = makeSymbol("unit", builtinDeclaration)

  override val PreludeScope: Scope = ScopeBuilder(None, mutable.HashMap(
    "Int" -> Primitive.IntSymbol,
    "Bool" -> Primitive.BoolSymbol,
    "Unit" -> Primitive.UnitSymbol,
    "Type" -> Primitive.TypeSymbol,
    "boolEq" -> _boolEqSymbol,
    "true" -> _trueSymbol,
    "false" -> _falseSymbol,
    "unit" -> _unitSymbol
  ))

  private val Prelude: Env = Env(
    types = Map(
      Primitive.BoolSymbol -> Primitive.Type,
      Primitive.IntSymbol -> Primitive.Type,
      Primitive.UnitSymbol -> Primitive.Type,
      Primitive.TypeSymbol -> Primitive.Type,
      _boolEqSymbol -> Value.arrow(Primitive.Bool, Primitive.Bool, Primitive.Bool),
      _trueSymbol -> Primitive.Bool,
      _falseSymbol -> Primitive.Bool,
      _unitSymbol -> Primitive.Unit
    ),
    values = Map(
      Primitive.BoolSymbol -> Primitive.Bool,
      Primitive.IntSymbol -> Primitive.Int,
      Primitive.TypeSymbol -> Primitive.Type,
      Primitive.UnitSymbol -> Primitive.Unit,
      _boolEqSymbol -> Value.ExternFunc(v1 => Value.ExternFunc(v2 => {
        Value.Bool(v1 == v2)
      })),
      _trueSymbol -> Value.Bool(true),
      _falseSymbol -> Value.Bool(false),
      _unitSymbol -> Value.Unit
    )
  )


  private val checker = new TypeChecker(this, Prelude)

  private def makePrimitive(symbol: Symbol): Type = {
    Constructor(symbol)
  }

  def compile(): Future[Unit] = {
    Future.unit
  }

  override def nextMetaId: Int = {
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
        val binder = new Renamer(this)
        binder.bind(parsed)
        val checkedSourceFile = checker.inferSourceFile(parsed, Prelude)
        cacheCheckedSourceFile(
          path,
          checkedSourceFile
        )
        checkedSourceFile
    }
  }

  def getSourceFileScope(path: Path): Scope = {
    getCheckedSourceFile(path).scope
  }

  def getSourceFileDiagnostics(
    path: Path
  ): Iterable[Diagnostic] = {
    getParsedSourceFile(path)
    getCheckedSourceFile(path)
    _sourceFileDiagnostics.getOrElseUpdate(path, ListBuffer.empty)
  }

  def getParsedSourceFile(path: Path): Parsed.SourceFile = {
    _parsedSourceFiles.get(path) match {
      case Some(p) =>
        p
      case None =>
        val chars = getCharStream(path)
        val tokens = Lexer(path, chars)
        val parser = new Parser(this, path, tokens)
        val sourceFile = parser.parseSourceFile()
        cacheParsedSourceFile(path, sourceFile)
        sourceFile
    }
  }

  def getCharStream(path: Path): Stream[Char] = {
    _charStreams.get(path) match {
      case Some(stream) =>
        debug(s"from cache: ${stream}")
        stream
      case None =>
        val stream = File(path)
        _charStreams.put(path, stream)
        stream
    }
  }

  def updateCharStream(path: Path, text: String): Unit = {
    _charStreams.update(path, StringStream(text))
    _parsedSourceFiles.remove(path)
    _typedSourceFiles.remove(path)
  }

  private def cacheParsedSourceFile(path: Path, sourceFile: Parsed.SourceFile) = {
    _parsedSourceFiles.put(path, sourceFile)
  }

  private def cacheCheckedSourceFile(path: Path, sourceFile: Typed.SourceFile) = {
    _typedSourceFiles.put(path, sourceFile)
  }


  override def makeSymbol(text: String, declaration: Parsed.Declaration): Symbol = {
    val id = _id
    _id += 1

    Symbol(id, text, declaration)
  }

  override def makeErrorSymbol(text: String): Symbol = {
    makeSymbol(text, _errorDeclaration)
  }

  def makeGenericType(name: String): Type = {
    val id = _nextGenericId
    _nextGenericId += 1
    ???
  }

  override def addError(diagnostic: Diagnostic): Unit = {
    _sourceFileDiagnostics.get(diagnostic.loc.path) match {
      case Some(errors) => errors.append(diagnostic)
      case None =>
        _sourceFileDiagnostics.update(diagnostic.loc.path, ListBuffer(diagnostic))
    }
  }

  def getHoverInfo(path: Path, pos: Pos): Option[String] = for {
    node <- findNodeAtPos(path, pos)
    info <- node match {
      case i: Typed.Ident =>
        getHoverInfoForIdent(i)
      case _ =>
        None
    }
  } yield info

  private def getHoverInfoForIdent(ident: Typed.Ident): Option[String] = {
    val symbol = ident.name
    ???
  }


  private def findNodeAtPos(path: Path, pos: Pos): Option[Typed.Node] = {
    findDeclAtPos(path, pos).flatMap(d => findNodeInNode(d, pos))
  }

  private def findDeclAtPos(path: Path, pos: Pos): Option[Typed.Declaration] = {
    val sourceFile = getCheckedSourceFile(path)
    sourceFile.declarations.find(
      d => d.loc.start <= pos && d.loc.end >= pos
    )
  }


  private def findNodeInNode(node: Typed.Node, pos: Pos): Option[Typed.Node] = {
    for (child <- node.children) {
      if (child.loc.start <= pos && child.loc.end >= pos) {
        return findNodeInNode(child, pos)
      }
    }
    if (node.loc.start <= pos && node.loc.end >= pos) {
      Some(node)
    } else {
      None
    }
  }

  def goToDefinition(path: Path, pos: Pos): Option[Loc] = {
    None
  }

  override def setParsedNode(id: Int, node: Parsed.Node): Unit =
    _parsedNodes.update(id, node)

  override def getParsedNode(id: Int): Option[Parsed.Node] =
    _parsedNodes.get(id)

  override def setDeclOf(symbol: Symbol, decl: Parsed.Declaration): Unit =
    _declarationOf.update(symbol, decl.meta.id)

  override def getTypeOfSymbol(symbol: Symbol): Type = {
    checker.getNormalizedType(symbol)
  }

  override def setType(symbol: Symbol, typ: Type): Unit = _types.update(symbol, typ)
}
