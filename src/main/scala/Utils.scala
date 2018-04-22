object Utils {
  def todo[T](message: String = "Unimplemented"): T = {
    throw new Exception(message)
  }
}