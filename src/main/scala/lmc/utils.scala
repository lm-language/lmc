package lmc

object utils {
  def todo[T](message: String = "Unimplemented"): T = {
    throw new Exception(message)
  }

  object Debug {
    private var _isDebugging = false
    def start(): Unit = {
      _isDebugging = true
    }

    def stop(): Unit = {
      _isDebugging = false
    }

    def isDebugging: Boolean = _isDebugging

    def log(x: Any): Unit = {
      if (_isDebugging) {
        println(x)
      }
    }
  }
  def joinIterable[T](iterable: Iterable[T], seperator: String = ","): String = {
    iterable.foldLeft("")((prev, current) => {
      prev
        .concat(seperator)
        .concat(current.toString)
    }).drop(seperator.length)
  }
}