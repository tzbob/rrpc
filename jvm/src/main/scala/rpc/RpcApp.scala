package rpc

import cats.effect.{Blocker, ExitCode, IO}
import cats.implicits._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s.server.staticcontent._

import scala.concurrent.ExecutionContext
import scala.io.Source

trait RpcApp extends RpcAppInt {
  override def run(args: List[String]): IO[ExitCode] = {
    val typed      = Infer.infer(rpc(args))
    val (_, store) = InterTerm.compileForInterpreter(typed)

    val resource = for {
      blocker <- Blocker[IO]
      server <- BlazeServerBuilder[IO]
        .bindHttp(port, hostname)
        .withHttpApp(Router(
          "/" -> ServerEvaluator.buildRoutes(typed, store, blocker)).orNotFound)
        .resource
    } yield server

    resource.use(_ => IO.never).as(ExitCode.Success)
  }
}
