package lmc
import java.nio.file.{ Path, Files }

object io {
  trait Stream[T] {
    def next: T
  }

  val EOF_CHAR: Char = (-1).toChar

  final case class StringStream(
    private val str: String
  ) extends Stream[Char] {
    private var index = 0
    override def next: Char = {
      if (index >= str.length) {
        EOF_CHAR
      } else {
        val c = str(index)
        index += 1
        c
      }
    }
  }

  object File {
    def apply(path: Path) = new File(path)
  }
  final class File(val path: Path) extends Stream[Char] {
    private val reader = Files.newBufferedReader(path)
    private var eofReached = false

    def readAllChars: String = {
      var str = ""
      var c = next
      while (c != EOF_CHAR) {
        str += c
        c = next
      }
      eofReached = true
      reader.close()
      str
    }

    def next: Char = {
      if (eofReached) {
        EOF_CHAR
      } else {
        val c = reader.read().toChar
        if (c == EOF_CHAR) {
          eofReached = true
          reader.close()
        }
        c
      }
    }
  }
}
