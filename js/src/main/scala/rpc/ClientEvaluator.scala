package rpc

import cats.effect.{ContextShift, IO}
import io.circe.{Decoder, Encoder}
import rpc.Declaration.TopLevel
import rpc.Expr.Open
import rpc.Interpreter.{CallInfo, RequestReplyF}
import sttp.client._
import sttp.client.circe._
import sttp.model.Uri

object ClientEvaluator {
  private implicit val backend = FetchBackend()

  private def caller[A: Encoder](uri: Uri)(a: A)(
      implicit ctx: ContextShift[IO]): IO[Either[CallInfo, Value]] = {

    import io.circe.generic.auto._
    implicit val decoder =
      implicitly[Decoder[CallInfo]] either implicitly[Decoder[Value]]

    println(s"calling server: payload $a, url: $uri")

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

  def buildClientRun(
      declarations: List[TopLevel[Open.Expr]],
      uri: Uri,
      appName: String)(implicit ctx: ContextShift[IO]): IO[Env] = {
    import io.circe.generic.auto._

    Interpreter
      .runDeclarations(
        declarations,
        RequestReplyF(caller[CallInfo](uri"$uri/$appName/interpret"),
                      caller[Value](uri"$uri/$appName/continue")))
  }
}
