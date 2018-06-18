package lmc

import java.nio.file.{Path, Paths}

import lmc.common._

import scala.concurrent.Future
import scala.collection._
import lmc.types.{ExistentialInstance, Kind, Type}
import diagnostics._
import io.{File, Stream, StringStream}
import lmc.syntax.{Named, Parsed, Typed}

import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference

class Compiler(paths: Iterable[Path], debug: (String) => Unit = (_) => {})
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
  private val _typeVariables = mutable.WeakHashMap.empty[Symbol, (Type, Kind)]
  private val _parsedNodes = mutable.WeakHashMap.empty[Int, Parsed.Node]
  private val _declarationOf = mutable.HashMap.empty[Symbol, Int]

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

  val checker = new TypeChecker(this)

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
      checker.setTypeOfSymbol(symbol, typ)
    }
    for ((name, (t, kind)) <- primitiveTypes) {
      val symbol = t.symbol
      scopeBuilder.setTypeVar(name, TypeEntry(symbol))
      setTypeVar(symbol, t, kind)
    }
    scopeBuilder
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
        val binder = new Binder(this)
        binder.bind(parsed)
        val checkedSourceFile = checker.inferSourceFile(parsed)
        checker.solveConstraints()
        cacheCheckedSourceFile(path, checkedSourceFile)
        checkedSourceFile
    }
  }

  def getType(symbol: Symbol): Option[Type] = {
    checker.getTypeOfSymbol(symbol).map(checker.applyEnv)
  }


  def setTypeVar(symbol: Symbol, typ: Type, kind: Kind): Unit = {
    _typeVariables.update(symbol, typ -> kind)
    checker.setKindOfSymbol(symbol, kind)
    checker.setTypeVar(symbol, typ)
  }

  def getKindOfSymbol(symbol: Symbol): Option[Kind] = {
    checker.getKindOfSymbol(symbol)
  }


  def getSourceFileScope(path: Path): Scope = {
    getCheckedSourceFile(path).scope
  }

  def getSourceFileDiagnostics(
    path: Path
  ): Iterable[Diagnostic] = {
    val parsedSourceFile = getParsedSourceFile(path)
    val parsedErrors = parsedSourceFile.errors
    val checkedSourceFile = getCheckedSourceFile(path)
    parsedErrors ++ checkedSourceFile.errors
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


  override def makeSymbol(text: String): Symbol = {
    val id = _id
    _id += 1

    Symbol(id, text)
  }

  def makeGenericType(name: String): Type = {
    val id = _nextGenericId
    _nextGenericId += 1
    ExistentialInstance(id, name)
  }

  def getHoverInfo(path: Path, pos: Pos): Option[String] = for {
    node <- findNodeAtPos(path, pos)
    info <- node match {
      case i: Typed.Ident =>
        getHoverInfoForIdent(i)
      case _ => None
    }
  } yield info

  private def getHoverInfoForIdent(ident: Typed.Ident): Option[String] = {
    val symbol = ident.name

    checker.getTypeOfSymbol(symbol).map(checker.applyEnv).map(_.toString) match {
      case Some(t) => Some(t)
      case None =>
        checker.getKindOfSymbol(ident.name).map(_.toString)
    }
  }



  private def inType(node: Parsed.Node): Boolean = {
    node match {
      case _: Parsed.TypeAnnotation => true
      case _ => (for {
        parentId <- node.meta.parentId
        parent <- getParsedNode(parentId)
        isInType = inType(parent)
      } yield isInType).contains(true)
    }

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

  override def getDeclOf(symbol: Symbol): Option[Parsed.Declaration] =
    _declarationOf.get(symbol).flatMap(getParsedNode)
      .asInstanceOf[Option[Parsed.Declaration]]

  override def setDeclOf(symbol: Symbol, decl: Parsed.Declaration): Unit =
    _declarationOf.update(symbol, decl.meta.id)
}
