package rpc

import java.util.UUID

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

    implicit val decoder   = jsonOf[IO, Identified[CallInfo]]
    implicit val vdencoder = jsonOf[IO, Identified[Value]]

    // TODO, use IOREF or another proper construct
    val sessionQueues = mutable.HashMap.empty[UUID, mutable.Queue[Cont[Value]]]
    def getOrAddQueue(id: UUID) =
      sessionQueues.getOrElseUpdate(id, mutable.Queue.empty)

    def respond(id: UUID, result: Either[ExternalCall, Value]) = result match {
      case Right(response) => Ok(response.asJson)
      case Left(ExternalCall(callInfo, cont)) =>
        val q = getOrAddQueue(id)
        q.enqueue(cont)
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
                <body><div id=body><code>$program</code>
                <h2>Evaluated to:</h2>
                <h1 id='result'>Calculating...</h1>
                </div></body>
                </html>
            """,
            `Content-Type`(MediaType.text.html)
          )

        case req @ POST -> Root / "interpret" =>
          for {
            idCallInfo <- req.as[Identified[CallInfo]]
            resp <- respond(
              idCallInfo.id,
              Interpreter.performServerRequest(idCallInfo.value)(store))
          } yield resp

        case req @ GET -> Root / "load" => Ok(program)

        case req @ POST -> Root / "continue" =>
          for {
            idValue <- req.as[Identified[Value]]
            resp <- respond(
              idValue.id,
              Interpreter.handleClientResponse[IO](idValue.value,
                                                   getOrAddQueue(idValue.id)))
          } yield resp
      }
  }
}
