package rpc

import io.circe.generic.JsonCodec

@JsonCodec
sealed trait Location
object Location {
  case class Var(name: String) extends Location
  case class Loc(name: String) extends Location
  val client = Loc("client")
  val server = Loc("server")
}
