package rpc

import cats.effect.IOApp

trait RpcAppInt extends IOApp {
  def rpc(args: List[String]): Expr.Open.Expr
  val hostname: String
  val port: Int
}
