package lmc

object utils {
  def todo[T](message: String = "Unimplemented"): T = {
    throw new Exception(message)
  }
}