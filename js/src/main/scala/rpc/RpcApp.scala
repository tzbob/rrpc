package rpc

import cats.effect.{ExitCode, IO, IOApp}
import sttp.client._

import org.scalajs.dom.document

trait RpcApp extends RpcAppInt {
  override def run(args: List[String]): IO[ExitCode] = {
    val typed         = Infer.infer(rpc(args))
    val (term, store) = InterTerm.compileForInterpreter(typed)

    println("Started")

    ClientEvaluator
      .buildClientRun(store, term, uri"http://$hostname:$port")
      .map { x =>
        println(s"Result: $x")
        document.getElementById("result").innerHTML = x.toString
        ExitCode.Success
      }
  }
}
