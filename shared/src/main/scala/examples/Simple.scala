package examples

import rpc._

object Simple extends RpcApp with RpcAppInt {
  val hostname = "localhost"
  val port     = 8080

  val apps = Map.empty
}
