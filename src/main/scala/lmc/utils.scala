package lmc

object utils {
  def todo[T](message: String = "Unimplemented"): T = {
    throw new Exception(message)
  }

  object Debug {
    private var isDebugging = false
    def start(): Unit = {
      isDebugging = true
    }

    def stop(): Unit = {
      isDebugging = false
    }

    def log(x: Any): Unit = {
      if (isDebugging) {
        println(x)
      }
    }
  }
}