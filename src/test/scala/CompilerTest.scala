import java.nio.file.Paths
import java.io.{File => JFile}

import IO._
import Compiler.Compiler
import org.junit.Test

import scala.concurrent.ExecutionContext.Implicits.global
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._
import org.scalatest.Assertions._
import better.files

class CompilerTest {
  @Test def compileTest = {
    val file = new JFile(".")
    val basePath = Paths.get(file.getAbsolutePath).getParent
    val suitePath = Paths.get(basePath.toString, "test", "compiler")
    val testOutPath = Paths.get(basePath.toString, "test", "compiler", "local")

    val compiler = new Compiler(paths = List(suitePath))
    val suiteDir = new JFile(suitePath.toString)
    for (path <- suiteDir.listFiles() filter { _.getName endsWith ".lm" }) {
      val filePath = Paths.get(path.getAbsolutePath)
      val symbolFileName = filePath.getFileName.toString dropRight 3 concat ".symbols.json"
      val symbolPath = filePath.resolveSibling(symbolFileName)
      val symbolFile = File(symbolPath)
      val symbolJSONStr = symbolFile.readAllChars
      val expectedScopeJSON = decode[ScopeJSON](symbolJSONStr)
      val sourceFile = compiler.getCheckedSourceFile(filePath)
      val compiledScopeJSON = ScopeJSON.fromScope(sourceFile.scope)

      files.File(testOutPath.resolve(symbolFileName)).writeText(compiledScopeJSON.asJson.spaces2)
      assertResult(expectedScopeJSON)(compiledScopeJSON)
    }
  }


  case class PosJSON(
    line: Int,
    column: Int
  )
  object PosJSON {
    def fromPos(pos: Pos): PosJSON = {
      PosJSON(pos.line, pos.column)
    }
  }
  case class LocJSON(
    start: PosJSON,
    end: PosJSON
  )
  object LocJSON {
    def fromLoc(loc: Loc): LocJSON = {
      LocJSON(PosJSON.fromPos(loc.start), PosJSON.fromPos(loc.end))
    }
  }
  case class SymbolEntryJSON(
    typ: String
  )
  case class ScopeJSON(
    loc: LocJSON,
    symbols: Map[String, SymbolEntryJSON],
    children: Array[ScopeJSON]
  )
  object ScopeJSON {
    def fromScope(scope: Scope): ScopeJSON = ScopeJSON(
      loc = LocJSON.fromLoc(scope.loc),
      symbols = scope.symbols.mapValues((t) => SymbolEntryJSON(t.typ.toString)),
      children = scope.children.map(fromScope).toArray
    )
  }
}