import java.nio.file.Paths
import java.io.File

class Suite {
  def runSuite(): Unit = {
    val file = new File(".")
    val basePath = Paths.get(file.getAbsolutePath()).getParent()
    val suitePath = Paths.get(basePath.toString(), "test", "suite")
    val stdlibPath = Paths.get(basePath.toString(), "stdlib")
  }
}