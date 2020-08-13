import java.io.{FileOutputStream, PrintStream}
import bsq._
import org.scalatest.funsuite.AnyFunSuite
import scala.sys.process._

class BsqTests extends AnyFunSuite {
  def executeMain(args: Array[String]) : Option[String] = {
    System.setOut(new PrintStream(new FileOutputStream("file.out")))

    try {
      Bsq.main(args)
      val fd = new FileHandler
      val output = fd.getFile(Option("file.out"))
      System.out.close()
      if ("rm file.out".! == 0) Some(output.toString) else None
    } catch {
      case e: Throwable => System.out.close(); None
    }
  }

  test("Too many arguments.") {
    val args = Array("lol", "fortnite", "gamer")
    val answer: Option[String] = executeMain(args)

    assert(answer.nonEmpty, "An error message should be detected.")
  }

  test("No arguments.") {
    val args = Array("")
    val answer: Option[String] = executeMain(args)

    assert(answer.nonEmpty, "An error message should be detected.")
  }

  test("wrong path.") {
    val args = Array("asd")
    val answer: Option[String] = executeMain(args)

    assert(answer.nonEmpty, "An error message should be detected.")
  }

  test("correct path.") {
    val args = Array("./maps/intermediate_map_100_100")
    val answer: Option[String] = executeMain(args)

    assert(answer.nonEmpty, "An error message should be detected.")

  }
}