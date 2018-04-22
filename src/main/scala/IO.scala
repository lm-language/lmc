import java.nio.file.{
  Path, Files
}
import java.nio.file.StandardOpenOption._
import java.io.{
  FileInputStream, BufferedReader, FileReader,
  BufferedInputStream, IOException
}

object IO {
  trait Stream[T] {
    def next: T
  }

  val EOF = -1.toChar
  object File {
    def apply(path: Path) = new File(path)
  }
  final class File(val path: Path) extends Stream[Char] {
    private val reader = Files.newBufferedReader(path)
    private var eofReached = false

    def readAllChars: String = {
      var str = ""
      var c = next
      while (c != EOF) {
        str += c
        c = next
      }
      eofReached = true
      reader.close()
      str
    }

    def next: Char = {
      if (eofReached) {
        EOF
      } else {
        val c = reader.read().toChar
        if (c == EOF) {
          eofReached = true
          reader.close()
        }
        c
      }
    }
  }
}