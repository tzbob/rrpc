package rpc

import io.circe.generic.JsonCodec

@JsonCodec
sealed trait Literal
object Literal {
  case class Int(i: scala.Int)           extends Literal
  case class String(s: java.lang.String) extends Literal
  case class Bool(b: Boolean)            extends Literal
  case object Unit                       extends Literal
}
