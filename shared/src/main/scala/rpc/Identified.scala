package rpc

import java.util.UUID

import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor, Json}

case class Identified[A](id: UUID, value: A)

object Identified {
  implicit val uuidEnc = Encoder.encodeString.contramap[UUID](_.toString)
  implicit val uuidDec = Decoder.decodeString.map(UUID.fromString)

  implicit def idEnc[A: Encoder] = new Encoder[Identified[A]] {
    override def apply(a: Identified[A]): Json = Json.obj(
      ("id", uuidEnc.apply(a.id)),
      ("value", implicitly[Encoder[A]].apply(a.value))
    )
  }
  implicit def idDec[A: Decoder] = new Decoder[Identified[A]] {
    override def apply(c: HCursor): Result[Identified[A]] =
      for {
        uuid  <- c.downField("id").as[UUID]
        value <- c.downField("value").as[A]
      } yield Identified(uuid, value)
  }
}
