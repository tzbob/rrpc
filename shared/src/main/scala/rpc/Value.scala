package rpc

import rpc.Expr.Closed.{LamRef, LibRef}
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.generic.auto._

@JsonCodec sealed trait Value

object Value {
  case class Constant(lit: Literal) extends Value
  case class Closure(ref: Either[LibRef, LamRef],
                     env: Map[Expr.Closed.Var, Value])
      extends Value

  implicit val keyEncoder = new KeyEncoder[Expr.Closed.Var] {
    override def apply(key: Expr.Closed.Var): String = key.name
  }

  implicit val keyDecoder = new KeyDecoder[Expr.Closed.Var] {
    override def apply(key: String): Option[Expr.Closed.Var] =
      Some(Expr.Closed.Var(key))
  }
}
