package rpc

import cats.effect.{ExitCode, IO}
import org.http4s.server.Router
import cats.implicits._
import org.http4s.server.blaze._
import org.http4s.implicits._
import org.http4s.server.Router

trait RpcApp extends RpcAppInt {
  override def run(args: List[String]): IO[ExitCode] = {

    val typed      = Infer.infer(rpc(args))
    val (_, store) = InterTerm.compileForInterpreter(typed)

    val service = ServerEvaluator.buildRoutes(store)
    val app     = Router("/" -> service).orNotFound
    BlazeServerBuilder[IO]
      .bindHttp(8080, hostname)
      .withHttpApp(app)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
