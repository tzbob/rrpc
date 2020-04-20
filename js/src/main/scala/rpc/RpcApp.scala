package rpc

import cats.effect.{ExitCode, IO}
import io.circe.parser._
import izumi.logstage.api.IzLogger
import org.scalajs.dom.document
import org.scalajs.dom.ext.Ajax
import rpc.Declaration.TopLevel
import rpc.Expr.Open

import scala.concurrent.ExecutionContext.Implicits.global

trait RpcApp extends RpcAppInt {
  private val logger = IzLogger()

  override def run(args: List[String]): IO[ExitCode] = {
    val pathName = document.location.pathname.split("/")
    val appName  = pathName.last
    val pageMode = document.location.hash == "#page"

    val ioRequest = IO.fromFuture(IO {
      Ajax.get(s"http://$hostname:$port/$appName/load")
    })

    val ioTopLevel = ioRequest.map { x =>
      import HaskellJsonDecoders._
      decode[List[TopLevel[Open.Expr]]](x.responseText) match {
        case Left(err)   => throw new RuntimeException(err.toString)
        case Right(tops) => tops
      }
    }

    if (pageMode) {
      logger.info(s"Starting page mode")
      ioTopLevel
        .flatMap(
          ClientEvaluator
            .buildPageRun(_, s"http://$hostname:$port", appName))
        .map(_ => ExitCode.Success)
    } else {
      logger.info(s"Starting value mode")
      val env = ioTopLevel.flatMap(
        ClientEvaluator.buildClientRun(_, s"http://$hostname:$port", appName))

      env.map { x =>
        val main = x.value("main")
        println(s"Result: $main")
        document.getElementById("result").innerHTML = main.toString
        ExitCode.Success
      }
    }
  }
}
