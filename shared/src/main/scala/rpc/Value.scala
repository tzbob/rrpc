package rpc

import rpc.Expr.Closed.{LamRef, LamStore}
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.syntax._
import rpc.Expr.Closed

sealed trait Value

object Value {
  case class Constant(lit: Literal)                     extends Value
  case class Closure(ref: LamRef, var env: Env.Minimal) extends Value
  object Closure {
    def addRecursiveClosure(name: String,
                            lamRef: LamRef,
                            env: Env,
                            store: LamStore): Env = {
      val closure = Closure(lamRef, null)
      val recEnv  = env.add(name, closure)
      val cl      = store(lamRef)
      val minimal = Env.minimize(recEnv, cl.tpeVars, cl.locVars, cl.freeVars)
      closure.env = minimal
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
