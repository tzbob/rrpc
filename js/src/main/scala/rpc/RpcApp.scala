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
      if (TopLevel.isPage(top)) {
        Log.logger.info(s"Starting page mode")
        ioTopLevel
          .flatMap(
            ClientEvaluator
              .buildPageRun(_, s"http://$hostname:$port", appName))
          .map(_ => ExitCode.Success)
      } else {
        Log.logger.info(s"Starting value mode")
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
}
