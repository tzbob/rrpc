package rpc

import cats.effect.{ExitCode, IO, IOApp}
import sttp.client._

import org.scalajs.dom.document

trait RpcApp extends RpcAppInt {
  override def run(args: List[String]): IO[ExitCode] = {
    val (closedExpr, store) = Expr.Closed.compileForInterpreter(rpc(args))

    println("Started")

    ClientEvaluator
      .buildClientRun(store, closedExpr, uri"http://$hostname:$port")
      .map { x =>
        println(s"Result: $x")
        document.getElementById("result").innerHTML = x.toString
        ExitCode.Success
      }
  }
}
