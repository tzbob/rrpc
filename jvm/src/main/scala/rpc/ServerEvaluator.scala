package rpc

import cats.effect.{ContextShift, IO}
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import org.http4s.{HttpRoutes, MediaType}
import rpc.Expr.Closed.LamStore
import rpc.Interpreter.{CallInfo, Cont, ExternalCall}

import scala.collection.mutable

object ServerEvaluator {

  def buildRoutes(program: String, store: LamStore)(
      implicit shift: ContextShift[IO]): HttpRoutes[IO] = {

    implicit val decoder  = jsonOf[IO, CallInfo]
    implicit val vdencoder = jsonOf[IO, Value]

    // TODO, use IOREF or another proper construct
    // FIXME: use sessions!
    val queue = mutable.Queue[Cont[Value]]()

    def respond(result: Either[ExternalCall, Value]) = result match {
      case Right(response) => Ok(response.asJson)
      case Left(ExternalCall(callInfo, cont)) =>
        queue.enqueue(cont)
        Ok(callInfo.asJson)
    }

    HttpRoutes
      .of[IO] {
        case GET -> Root =>
          Ok(
            s"""<!DOCTYPE html><head>
                <meta charset='UTF-8'>
                <script src='/assets/client.js'></script>
                </head>
                <body><code>$program</code>
                <h2>Evaluated to:</h2>
                <h1 id='result'>Calculating...</h1>
                </body>
                </html>
            """,
            `Content-Type`(MediaType.text.html)
          )

        case req @ POST -> Root / "interpret" =>
          for {
            callInfo <- req.as[CallInfo]
            resp     <- respond(Interpreter.performServerRequest(callInfo)(store))
          } yield resp

        case req @ GET -> Root / "load" => Ok(program)

        case req @ POST -> Root / "continue" =>
          for {
            value <- req.as[Value]
            resp  <- respond(Interpreter.handleClientResponse[IO](value, queue))
          } yield resp
      }
  }
}
