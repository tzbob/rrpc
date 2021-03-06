package rpc

import java.io.File

import scala.io.Source

object PolyRpcCaller {
  case class Config(path: String, extension: String, binPath: String)

  def toFileLocation(name: String)(implicit c: Config) =
    s"${c.path}/$name.${c.extension}"
  def toJsonLocation(name: String)(implicit c: Config) =
    s"${c.path}/$name.json"

  // TODO: write this using non-blocking and error-handling IO
  def load(name: String, reset: Boolean = false)(implicit c: Config): String = {
    val js = new File(toJsonLocation(name))
    if (reset) js.delete()
    if (!js.exists()) {
      println(js)
      println(generateTypedAST(name))
    }
    readTypedAST(name)
  }

  def readTypedAST(name: String)(implicit c: Config) = {
    val json = toJsonLocation(name)
    Source.fromFile(json).getLines().mkString("\n")
  }

  private def generateTypedAST(name: String)(implicit c: Config) = {
    val path     = s"${c.binPath}/${OsUtil.simpleOs()}"
    val bin      = s"$path/polyrpc-exe${OsUtil.binSuffix()}"
    val location = toFileLocation(name)
    os.proc(bin, "--output-json", location).call(mergeErrIntoOut = true)
  }
}
