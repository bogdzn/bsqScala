package bsq

class FileContent(filenameOption: Option[String]) {
  def isEmpty(option: Option[Array[String]]): Boolean = option.isEmpty

  def get(filenameOption: Option[String]): Option[Array[String]] = {
    def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
      try f(resource) finally resource.close()

    val filename: String = filenameOption.getOrElse("No name given.")
    try {
      val lines = using(io.Source.fromFile(filename)) { source =>
      (for (line <- source.getLines) yield line).toArray }
      Some(lines)
    }
    catch { case e: Throwable => None }
  }

  val content: Option[Array[String]] = get(filenameOption)
}
