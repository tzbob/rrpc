package rpc

import cats.syntax.functor._
import io.circe.generic.JsonCodec
import io.circe.{Decoder, HCursor}

sealed trait Location
object Location {
  case class Var(name: String) extends Location
  case class Loc(name: String) extends Location
  val client = Loc("client")
  val server = Loc("server")

  implicit val locVarD: Decoder[Var] = (c: HCursor) =>
    c.downField("LocVar").as[String].map(Var)
  implicit val locLocD: Decoder[Loc] = (c: HCursor) =>
    c.downField("Location").as[String].map(Loc.apply)
  implicit val locD: Decoder[Location] =
    List[Decoder[Location]](locVarD.widen, locLocD.widen).reduceLeft(_ or _)
}

//sealed trait Location { def superscript: String }
//
//object Location {
//  case object Server extends Location { val superscript = "ˢ" }
//  case object Client extends Location { val superscript = "ᶜ" }
//}
