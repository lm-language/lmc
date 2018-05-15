package lmc

import java.io.{BufferedReader, BufferedWriter, FileReader, InputStream, InputStreamReader, OutputStream, OutputStreamWriter, PrintWriter, File => JFile}
import java.net._
import java.nio.channels._

class Server {
  private val in = new BufferedReader(new InputStreamReader(System.in))
  private val out = new OutputStreamWriter(System.out)
  def listen(): Unit = {
    var done = false
    while (!done) {
      val message = readMessage()
      Thread.sleep(100)
    }
    Runtime.getRuntime.exit(0)
  }

  def readMessage(): Unit = {
    val header = in.readLine()
    val headerRegex = "Content-Length:\\s*(\\d+)".r
    val matches = headerRegex.findFirstMatchIn(header)
    matches match {
      case Some(value) =>
        in.readLine()
        val length = value.group(1).toInt
        val buffer: Array[Char] = Array.fill(length)('0')
        in.read(buffer, 0, length)
        val messageStr = buffer.foldLeft("")(_ concat _.toString)
        val outMessage = """{"jsonrpc":"2.0","id":0,"method":"initialize","capabilities":{"hoverProvider":true}}"""
        out.write(s"Content-Length: ${outMessage.length}\r\n")
        out.write(outMessage)
      case None =>
        ()
    }

  }
}
