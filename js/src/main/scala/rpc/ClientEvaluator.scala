package rpc

import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import io.circe.{Decoder, Encoder}
import rpc.Expr.Closed.LamStore
import rpc.Interpreter.CallInfo
import sttp.client._
import sttp.client.circe._
import sttp.model.Uri

object ClientEvaluator {
  private implicit val backend = FetchBackend()

  private def caller[A: Encoder](uri: Uri)(a: A)(
      implicit ctx: ContextShift[IO]): IO[Either[CallInfo, Value]] = {

    implicit val decoder =
      implicitly[Decoder[CallInfo]] either implicitly[Decoder[Value]]

    val responseIO = IO.fromFuture(IO {
      basicRequest
        .body(a)
        .post(uri)
        .response(asJson[Either[CallInfo, Value]])
        .send()
    })

    responseIO.map { x =>
      x.body match {
        case Left(err)  => throw err
        case Right(res) => res
      }
    }
  }

  def buildClientRun(store: LamStore, expr: Expr.Closed.Expr, uri: Uri)(
      implicit ctx: ContextShift[IO]): IO[Value] = {
    Interpreter.runClient(expr, store, Map.empty)(
      caller[CallInfo](uri"$uri/interpret"))(caller[Value](uri"$uri/continue"))
  }
}
