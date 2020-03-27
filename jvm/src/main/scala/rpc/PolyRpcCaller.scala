package rpc

import java.io.File

import scala.io.Source
import scala.sys.process.Process

object PolyRpcCaller {

  case class Config(path: String, extension: String)

  def toFileLocation(name: String)(implicit c: Config) =
    s"${c.path}/$name.${c.extension}"
  def toJsonLocation(name: String)(implicit c: Config) =
    s"${toFileLocation(name)}.json"

  // TODO: write this using non-blocking and error-handling IO
  def load(name: String, reset: Boolean = false)(implicit c: Config): String = {
    val js = new File(toJsonLocation(name))
    if (reset) js.delete()
    if (!js.exists())
      println(generateTypedAST(name))
    readTypedAST(name)
  }

  def readTypedAST(name: String)(implicit c: Config) = {
    val json = toJsonLocation(name)
    Source.fromFile(json).getLines().mkString("\n")
  }

  private def generateTypedAST(name: String)(implicit c: Config) = {
    val bin      = "jvm/src/main/resources/polyrpc-exe"
    val location = toFileLocation(name)
    Process(s"$bin --output-json $location").lazyLines.mkString("\n")
  }
}
