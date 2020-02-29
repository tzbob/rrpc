package rpc

import cats.effect.IOApp

trait RpcAppInt extends IOApp {
  def rpc(args: List[String]): Term
  val hostname: String
}
