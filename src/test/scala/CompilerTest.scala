import java.nio.file.Paths
import java.io.{File => JFile}
import java.io.{BufferedReader, InputStreamReader}

import org.junit.Test
import better.files
import lmc.io._
import lmc._
import net.liftweb.json
import lmc.common._

class CompilerTest {
  implicit val formats: json.DefaultFormats = json.DefaultFormats

  @Test def compileTest = {
    val envDiff = System.getenv("DIFF")
    val diff =
      if (envDiff == null)
        "diff"
      else
        envDiff

    val file = new JFile(".")
    val basePath = Paths.get(file.getAbsolutePath).getParent
    val suitePath = Paths.get(basePath.toString, "test", "compiler")
    val testOutPath = Paths.get(basePath.toString, "test", "compiler", "local")

    val suiteDir = new JFile(suitePath.toString)

    val compiler = new Compiler(paths = List(suitePath))
    for (path <- suiteDir.listFiles()
        filter { _.getName endsWith ".lm" }
        filter { _.getName contains "BasicDependent" }
    ) {
      val filePath = Paths.get(path.getAbsolutePath)
      println(s"Checking $filePath")
      val diagnosticsFileName = filePath.getFileName.toString dropRight 3 concat ".diagnostics.json"
      val diagnosticsFilePath = filePath.resolveSibling(diagnosticsFileName)
      val diagnosticsFile = File(diagnosticsFilePath)
      val diagnosticsJSONStr = diagnosticsFile.readAllChars
      val parsedDiagnosticsJSON = json.parse(diagnosticsJSONStr)
      //      println(diagnosticsJSON)
      //      val diagnosticsJSON = decode[DiagnosticsJSON](diagnosticsJSONStr)
      val diagnostics = compiler.getSourceFileDiagnostics(filePath)
      val compiledDiagnosticsJSON = DiagnosticsJSON.fromDiagnostics(diagnostics)
      val outputDiagnosticFile = testOutPath.resolve(diagnosticsFileName)
      val compiledDiagnosticsJSONStr = json.Serialization.writePretty(compiledDiagnosticsJSON)
      val compiledDiagnosticsJSONV = json.parse(json.Serialization.writePretty(compiledDiagnosticsJSON))
      files.File(outputDiagnosticFile).writeText(compiledDiagnosticsJSONStr)

      if (parsedDiagnosticsJSON != compiledDiagnosticsJSONV) {
        println(Console.RED)
        println(s"""Errors don't match for $filePath""")
        println(Console.RESET)
        showDiff(diagnosticsFilePath.toString, outputDiagnosticFile.toString)
      }

      val symbolFileName = filePath.getFileName.toString dropRight 3 concat ".symbols.json"
      val symbolPath = filePath.resolveSibling(symbolFileName)
      val symbolFile = File(symbolPath)
      val symbolJSONStr = symbolFile.readAllChars
      val parsedExpectedScope = json.parse(symbolJSONStr)
      val sourceFile = compiler.getParsedSourceFile(filePath)
      val compiledScopeJSON = ScopeJSON.fromScope(compiler)(sourceFile.scope)
      val outputScopeFile = testOutPath.resolve(symbolFileName)
      val compiledScopeJSONString = json.Serialization.writePretty(compiledScopeJSON)
      val compiledScopeJSONV = json.parse(compiledScopeJSONString)
      files.File(outputScopeFile).writeText(compiledScopeJSONString)
      if (parsedExpectedScope != compiledScopeJSONV) {
        println(Console.RED)
        println(s"""Symbols don't match for $symbolPath and $outputScopeFile""")
        println(Console.RESET)
        showDiff(
          symbolPath.toString, outputScopeFile.toString
        )
      }
    }
  }

  private def showDiff(expected: String, found: String): Unit = {
    val envDiff = System.getenv("DIFF")
    val command = if (envDiff == null) "diff" else envDiff
    runCommand(s"$command $expected $found")
  }

  def runCommand(command: String): Unit = {
    val proc = Runtime.getRuntime.exec(command)

    val stdInput = new BufferedReader(new
      InputStreamReader(proc.getInputStream))

    val stdError = new BufferedReader(new
     InputStreamReader(proc.getErrorStream))

    var s: String = null
    while ({
      s = stdInput.readLine()
      s != null
    }) {
        System.out.println(s)
    }

    // read any errors from the attempted command
    while ({
      s = stdError.readLine()
      s != null
    }) {
        System.out.println(s)
    }
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
  children: List[ScopeJSON]
)
object ScopeJSON {
  def fromScope(compiler: Compiler)(scope: Scope): ScopeJSON = {
    val symbols = scope.symbols
    ScopeJSON(
      loc = LocJSON.fromLoc(scope.loc),
      symbols = symbols
        .mapValues(s => {
          compiler.getTypeOfSymbol(s).toString
        })
        .mapValues(SymbolEntryJSON)
        .toMap,
      children = scope.children
        .map(_.get)
        .map(_.orNull)
        .filter(_ != null)
        .map(fromScope(compiler))
        .toList
        .reverse
    )
  }
}


case class DiagnosticsJSON(
  diagnostics: List[DiagnosticJSON]
)

object DiagnosticsJSON {
  def fromDiagnostics(diagnostics: Iterable[lmc.diagnostics.Diagnostic]): DiagnosticsJSON =
    DiagnosticsJSON(
      diagnostics = diagnostics
        .toList
        .sortWith((a, b) => a.loc.start.lt(b.loc.start))
        .map((d) => DiagnosticJSON(
          loc = LocJSON.fromLoc(d.loc),
          severity = d.severity.toString,
          message = d.message
        )).toList
    )

}
case class DiagnosticJSON(
  loc: LocJSON,
  severity: String,
  message: String
)
