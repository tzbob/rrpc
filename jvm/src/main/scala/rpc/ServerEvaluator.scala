package rpc

import java.io.File

import cats.effect.{Blocker, ContextShift, IO}
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import org.http4s.{HttpRoutes, MediaType, StaticFile}
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import rpc.InterTerm.LamStore
import rpc.Interpreter.{CallInfo, Cont, ExternalCall}
import rpc.TypedTerm.PrettyShow
import org.http4s.server.staticcontent._

import scala.collection.mutable

object ServerEvaluator {

  def buildRoutes(term: TypedTerm, store: LamStore, blocker: Blocker)(
      implicit shift: ContextShift[IO]): HttpRoutes[IO] = {
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
        case GET -> Root =>
          Ok(
            s"""<!DOCTYPE html><head>
                <meta charset='UTF-8'>
                <script src='assets/client.js'></script>
                </head>
                <body><h1>${PrettyShow.show(term)}</h1>
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
            resp     <- respond(Interpreter.performServerRequest(store, callInfo))
          } yield resp

        case req @ POST -> Root / "continue" =>
          for {
            value <- req.as[Value]
            resp <- respond(
              Interpreter.handleClientResponse[IO](store, value, queue))
          } yield resp

        case req @ GET -> Root / "assets" / "client.js" =>
          // StaticFile.fromResource[IO]("test.html", blocker, Some(req)).getOrElseF(NotFound())
          StaticFile
            .fromFile(new File("../js/target/scala-2.13/rpc-opt.js"),
                      blocker,
                      Some(req))
            .getOrElseF(NotFound())

      }
  }
}
