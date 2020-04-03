package rpc

import cats.effect.{ExitCode, IO}
import io.circe.parser._
import org.scalajs.dom.document
import rpc.Declaration.TopLevel
import rpc.Expr.Open
import sttp.client._

trait RpcApp extends RpcAppInt {
  private implicit val backend = FetchBackend()

  override def run(args: List[String]): IO[ExitCode] = {
    println("Started")

    val pathName = document.location.pathname
    val appName  = pathName.split("/").last

    val ioRequest = IO.fromFuture(IO {
      basicRequest.get(uri"http://$hostname:$port/$appName/load").send()
    })

    val ioTopLevel = ioRequest.map { x =>
      import HaskellJsonDecoders._
      x.body.flatMap(decode[List[TopLevel[Open.Expr]]]) match {
        case Left(err)   => throw new RuntimeException(err.toString)
        case Right(tops) => tops
      }
    }

    val env = ioTopLevel.flatMap(
      ClientEvaluator.buildClientRun(_, uri"http://$hostname:$port", appName))

    env.map { x =>
      val main = x.value("main")
      println(s"Result: $main")
      document.getElementById("result").innerHTML = main.toString
      ExitCode.Success
    }
  }
}
