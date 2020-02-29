package rpc

import cats.effect.{ExitCode, IO, IOApp}
import sttp.client._

trait RpcApp extends RpcAppInt {
  override def run(args: List[String]): IO[ExitCode] = {

    val typed         = Infer.infer(rpc(args))
    val (term, store) = InterTerm.compileForInterpreter(typed)

    ClientEvaluator
      .buildClientRun(store, term, uri"$hostname")
      .map(_ => ExitCode.Success)
  }
}
