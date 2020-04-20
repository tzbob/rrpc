package examples

import rpc._
import io.circe.parser._
import rpc.Expr.Open

object Simple extends RpcApp with RpcAppInt {
  val hostname = "localhost"
  val port     = 8080
}
