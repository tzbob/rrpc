package rpc

import rpc.Expr.Closed.{LamRef}
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.generic.auto._
import io.circe.syntax._
import rpc.Expr.Closed

@JsonCodec sealed trait Value

object Value {
  case class Constant(lit: Literal)             extends Value
  case class Closure(ref: LamRef, var env: Env) extends Value
  object Closure {
    def addRecursiveClosure(name: String, lamRef: LamRef, env: Env): Env = {
      val closure = Closure(lamRef, null)
      val recEnv  = env.add(name, closure)
      closure.env = recEnv
      recEnv
    }
  }
  case class Constructed(tag: String, args: List[Value]) extends Value
  case class Tupled(args: List[Value])                   extends Value

  implicit val keyEncoder = new KeyEncoder[Expr.Closed.Var] {
    override def apply(key: Expr.Closed.Var): String = key.name
  }

  implicit val keyDecoder = new KeyDecoder[Expr.Closed.Var] {
    override def apply(key: String): Option[Expr.Closed.Var] =
      Some(Expr.Closed.Var(key))
  }
}
