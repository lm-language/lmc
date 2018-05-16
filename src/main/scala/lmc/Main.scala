package lmc

object Main {
  val USAGE: String =
    s"""
       | LMC
       | Usage:
       | lmc [command] [compiler-options]
       | Commands
       | --------
       | server   Run language server on the current directory
     """.stripMargin
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println(USAGE)
    } else {
      args(0) match {
        case "server" =>
          val server = new Server()
          server.listen()
        case c =>
          println(s"Unknown command $c")
          println(USAGE)
      }
    }
  }
}
