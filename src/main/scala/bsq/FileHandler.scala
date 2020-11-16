package bsq

import io.Source.fromFile

case class FileHandler() {
  private def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try f(resource)
    finally resource.close()

  def read(filenameOption: Option[String]): Option[Array[String]] = {
    val filename: String = filenameOption.getOrElse("No name given.")
    try {
      val lines = using(fromFile(filename)) { source =>
        (for (line <- source.getLines) yield line).toArray
      }
      Some(lines)
    } catch { case e: Throwable => None }
  }
}
