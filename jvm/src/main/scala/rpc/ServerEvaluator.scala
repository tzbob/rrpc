package rpc

import cats.effect.IO
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.dsl.io._
import rpc.InterTerm.LamStore
import rpc.Interpreter.{CallInfo, Cont, ExternalCall}

import scala.collection.mutable

object ServerEvaluator {

  def buildRoutes(store: LamStore): HttpRoutes[IO] = {
    implicit val decoder  = jsonOf[IO, CallInfo]
    implicit val vencoder = jsonOf[IO, Value]

    // FIXME, use IOREF or another proper construct
    val queue = mutable.Queue[Cont[Value]]()

    def respond(result: Either[ExternalCall, Value]) = result match {
      case Right(response) => Ok(response.asJson)
      case Left(ExternalCall(callInfo, cont)) =>
        queue.enqueue(cont)
        Ok(callInfo.asJson)
    }

    HttpRoutes
      .of[IO] {
        case req @ POST -> Root / "interpret" =>
          for {
            callInfo <- req.as[CallInfo]
            resp     <- respond(Interpreter.performServerRequest(store, callInfo))
          } yield resp

        case req @ POST -> Root / "continue" =>
          for {
            value <- req.as[Value]
            resp <- respond(
              Interpreter.handleClientResponse[IO](store, value, queue))
          } yield resp
      }
  }
}
