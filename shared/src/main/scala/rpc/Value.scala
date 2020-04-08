package rpc

import rpc.Expr.Closed.{LamRef, LamStore}
import io.circe._
import io.circe.generic.JsonCodec

@JsonCodec sealed trait Value

object Value {
  case class Constant(lit: Literal)                     extends Value
  case class Closure(ref: LamRef, var env: Env.Minimal) extends Value
  object Closure {
    def addRecursiveClosure(name: String,
                            lamRef: LamRef,
                            envTarget: Env,
                            store: LamStore): Env = {
      val env     = envTarget.addRecursiveName(name)
      val cl      = store(lamRef)
      val minimal = Env.minimize(env, cl.tpeVars, cl.locVars, cl.freeVars)
      val closure = Closure(lamRef, minimal)
      env.add(name, closure)
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
