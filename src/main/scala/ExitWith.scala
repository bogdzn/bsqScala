package bsq

case class ExitWith(code: Int, error_message: String) {
  System.err.println(error_message)
  sys.exit(code)
}
