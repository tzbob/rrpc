package rpc

import rpc.InterTerm.LamRef
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.generic.auto._

@JsonCodec sealed trait Value

object Value {
  case class Const(i: Int) extends Value
  case class Closure(lamRef: LamRef, env: Map[InterTerm.Var, Value])
      extends Value

  implicit val keyEncoder = new KeyEncoder[InterTerm.Var] {
    override def apply(key: InterTerm.Var): String = key.idx.toString
  }

  implicit val keyDecoder = new KeyDecoder[InterTerm.Var] {
    override def apply(key: String): Option[InterTerm.Var] = ???
  }
}
