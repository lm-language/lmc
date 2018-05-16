package lmc

import java.io.{
  BufferedReader, BufferedWriter, FileReader,
  InputStream, InputStreamReader, OutputStream,
  OutputStreamWriter, PrintWriter, File => JFile
}
import java.nio.file.Path
import java.net._
import java.nio.channels._
import net.liftweb.json
import net.liftweb.json._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import scala.math.BigInt
import lmc.common._
import java.nio.file.Paths

object ErrorCodes {
	// Defined by JSON RPC
	val ParseError = -32700;
	val InvalidRequest = -32600;
	val MethodNotFound = -32601;
	val InvalidParams = -32602;
	val InternalError = -32603;
	val serverErrorStart = -32099;
	val serverErrorEnd = -32000;
	val ServerNotInitialized = -32002;
	val UnknownErrorCode = -32001;

	// Defined by the protocol.
	val RequestCancelled = -32800;
}

object MessageType {
	/**
	 * An error message.
	 */
	val Error = 1;
	/**
	 * A warning message.
	 */
	val Warning = 2;
	/**
	 * An information message.
	 */
	val Info = 3;
	/**
	 * A log message.
	 */
	val Log = 4;
}

class Server {
  private val in = new BufferedReader(new InputStreamReader(System.in))
  private val out = new OutputStreamWriter(System.out)
  private val err = new OutputStreamWriter(System.err)
  private var compiler: Compiler = null
  def listen(): Unit = {
    var done = false
    while (!done) {
        
      val header = in.readLine()
      val headerRegex = "Content-Length:\\s*(\\d+)".r
      val matches = headerRegex.findFirstMatchIn(header)
      matches match {
        case Some(matchValue) =>
          in.readLine()
          val length = matchValue.group(1).toInt
          val buffer: Array[Char] = Array.fill(length)('0')
          in.read(buffer, 0, length)
          val messageStr = buffer.foldLeft("")(_ concat _.toString)
          val messageJSON = json.parse(messageStr)
          LSP.LSPMessage.fromJSON(messageJSON) match {
            case Right(message) =>
              handleMessage(message)
            case Left(error) =>
              respond((
                (
                  ("error" -> error.toJValue)
                )
              ))
          }
        case None =>
          throw new Error(s"Invalid header $header")
      }
      Thread.sleep(100)
    }
    Runtime.getRuntime.exit(0)
  }

  def handleMessage(message: LSP.LSPMessage): Unit = (for {
    _ <- message.params match {
      case LSP.Shutdown =>
        Right({
          out.write("Shutting down server")
          Runtime.getRuntime.exit(0)
        })
      case LSP.Initialize(rootUri) => (for {
        path <- resolveUri(rootUri)
        id = message.id
        _ = {
          this.compiler = new Compiler(List(Paths.get(path)), (msg) => {
            respond((
              ("debug" -> msg)
            ))
          })
        }
      } yield respond((
        ("id" -> id) ~
        ("jsonrpc" -> "2.0") ~
        ("result" -> (
          ("capabilities" ->
            ("hoverProvider" -> true) ~
            ("textDocumentSync" -> (
              ("openClose" -> true) ~
              ("change" -> 1)
            ))
          )
        ))
      )))
    case msg@LSP.TextDocument.DidOpen(_, _, _, _) => for {
        path <- resolveUri(msg.uri)
      } yield syncDiagnostics(msg.uri, Paths.get(path))
    case msg@LSP.TextDocument.DidChange(_, _) => for {
        path <- resolveUri(msg.uri)
      } yield syncDiagnostics(msg.uri, Paths.get(path))
    case LSP.TextDocument.Hover(file, pos) => for {
        path <- resolveUri(file.uri)
        hoverInfo = compiler.getHoverInfo(Paths.get(path), pos)
      } yield respond((
          ("id" -> message.id) ~
          ("jsonrpc" -> "2.0") ~
          ("result" -> (
            "contents" -> hoverInfo
          ))
        ))
      case LSP.Workspace.DidChangeConfiguration =>
        Right(())
      case LSP.Initialized =>
        Right(())
    }
  } yield ()) match {
    case Left(err) =>
      respond((
        ("jsonrpc" -> "2.0") ~
        ("error" -> (
          ("message" -> err.message) ~
          ("data" -> err.data) ~
          ("code" -> ErrorCodes.ParseError)
        ))
      ))
    case Right(()) => ()
  }

  def syncDiagnostics(uri: String, path: java.nio.file.Path): Unit = {
    val errors = compiler.getSourceFileDiagnostics(path, true)
    respond((
      ("jsonrpc" -> "2.0") ~
      ("method" -> "textDocument/publishDiagnostics") ~
      ("params" -> (
        ("uri" -> uri) ~
        ("diagnostics" -> errors.map(renderDiagnostic).toList)
      ))
    ))
  }

  def renderDiagnostic(error: diagnostics.Diagnostic): JValue = {
    return (
      ("message" -> error.message) ~
      ("range" -> (
        ("start" -> (
          (
            ("line" -> (error.loc.start.line - 1)) ~
            ("character" -> (error.loc.start.column - 1))
          )
        )) ~
        ("end" -> (
          (
            ("line" -> (error.loc.end.line - 1)) ~
            ("character" -> (error.loc.end.column))
          )
        ))
      ))
    )
  }

  def resolveUri(uri: String): Either[LSP.Error, String] = {
    uri.take(7) match {
      case "file://" => Right(uri.drop(7))
      case scheme =>
        Left(LSP.Error(
          s"URI scheme $scheme isn't supported yet",
          ErrorCodes.InternalError
        ))
    }
  }

  def respond(v: JValue): Unit = {
    val response = json.compactRender(v)
    out.write(s"Content-Length: ${response.length}\r\n\r\n")
    out.write(response)
    out.flush()
  }
}

object LSP {
  implicit class JSON(j: JValue) {
    def getField(key: String): Option[JValue] = {
      j match {
        case JObject(fields) =>
          fields.find(_.name == key).map(_.value)
        case _ => None
      }
    }
  }
  case class LSPMessage(
    id: JValue,
    params: LSPParams
  )

  case class Error(
    message: String,
    code: Int,
    data: Option[JValue] = None
  ) {
    def toJValue: JValue = (
      ("error" -> (
        ("message" -> message) ~
        ("code" -> code) ~
        ("data" -> data)
      ))
    )
  }
  object LSPMessage {
    def fromJSON(json: JValue): Either[Error, LSPMessage] = {
      for {
        _ <- Right(JNull)
        id = json \ "id"
        JString(method) = json \ "method"
        params <- LSPParams.fromJSON(method, json)
      } yield LSPMessage(id, params)
    }
  }

  object LSPParams {
    def fromJSON(method: String, json: JValue): Either[Error, LSPParams] = {
      method match {
        case "initialized" => Right(Initialized)
        case "initialize" => Initialize.fromJSON(json)
        case "shutdown" => Right(Shutdown)
        case "textDocument/hover" =>
          TextDocument.Hover.fromJSON(json)
        case "workspace/didChangeConfiguration" =>
          Right(Workspace.DidChangeConfiguration)
        case "textDocument/didOpen" =>
          TextDocument.DidOpen.fromJSON(json)
        case "textDocument/didChange" =>
          TextDocument.DidChange.fromJSON(json)
        case _ =>
          Left(Error(
            message = s"Unhandled method $method",
            code = ErrorCodes.InternalError
          ))
      }
    }
  }
  sealed trait LSPParams
  case class Initialize(
    rootUri: String
  ) extends LSPParams

  object Initialize {
    def fromJSON(json: JValue): Either[Error, Initialize] = for {
      _ <- Right(Unit)
      JString(rootUri) = json \ "params" \ "rootUri"
    } yield Initialize(
      rootUri
    )
  }

  case object Shutdown extends LSPParams
  case object Initialized extends LSPParams

  object Workspace {
    case object DidChangeConfiguration extends LSPParams
  }

  case class TextDocumentIdentifier(uri: String)

  object TextDocument {

    case class DidOpen(
      uri: String,
      languageId: String,
      version: Int,
      text: String
    ) extends LSPParams
    object DidOpen {
      def fromJSON(j: JValue): Either[Error, DidOpen] = {
        val params = j \ "params" \ "textDocument"
        val JString(uri) = params \ "uri"
        val JString(languageId) = params \ "languageId"
        val JInt(version) = params \ "version"
        val JString(text) = params \ "text"
        Right(DidOpen(
          uri, languageId,
          version.toInt, text
        ))
      }
    }

    case class DidChange(
      uri: String,
      version: Int
    ) extends LSPParams
    object DidChange {
      def fromJSON(j: JValue): Either[Error, DidChange] = {
        val params = j \ "params" \ "textDocument"
        val JString(uri) = params \ "uri"
        val JInt(version) = params \ "version"
        Right(DidChange(
          uri,
          version.toInt
        ))
      }
    }



    case class Hover(
      textDocument: TextDocumentIdentifier,
      position: Pos
    ) extends LSPParams

    object Hover {
      def fromJSON(json: JValue): Either[Error, Hover] = {
        val JString(uri) = json \ "params" \ "textDocument" \ "uri"
        val JInt(line) = json \ "params" \ "position" \ "line"
        val JInt(character) = json \ "params" \ "position" \ "character"
        Right(Hover(
          textDocument = TextDocumentIdentifier(uri),
          position = Pos(line.toInt, character.toInt)
        ))
      }
    }
  }
}