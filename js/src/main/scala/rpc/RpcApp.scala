package rpc

import cats.effect.{ExitCode, IO}
import io.circe.parser._
import izumi.logstage.api.IzLogger
import org.scalajs.dom.document
import org.scalajs.dom.ext.Ajax
import rpc.Declaration.TopLevel
import rpc.Expr.Open

trait RpcApp extends RpcAppInt {
  override def run(args: List[String]): IO[ExitCode] = {
    val pathName = document.location.pathname.split("/")
    val appName  = pathName.last

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

    ioTopLevel.flatMap { top =>
      val url = s"http://$hostname:$port"
      if (TopLevel.isPage(top)) {
        Log.logger.info(s"Starting page mode")
        ioTopLevel
          .flatMap(ClientEvaluator.buildPageRun(_, url, appName))
          .map(_ => ExitCode.Success)
      } else if (TopLevel.isHtml(top)) {
        Log.logger.info(s"Starting html mode")
        ioTopLevel
          .flatMap(ClientEvaluator.buildHtmlRun(_, url, appName))
          .map(_ => ExitCode.Success)
      } else {
        Log.logger.info(s"Starting value mode")
        val env =
          ioTopLevel.flatMap(ClientEvaluator.buildClientRun(_, url, appName))

        env.map { x =>
          val main = x.value("main")
          println(s"Result: $main")
          document.getElementById("result").innerHTML = main.toString
          ExitCode.Success
        }
      }
    }
  }
}
