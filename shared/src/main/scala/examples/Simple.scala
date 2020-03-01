package examples

import rpc._
import Dsl._

object Simple extends RpcApp with RpcAppInt {

  val hostname = "localhost"
  val port = 8080

  def rpc(args: List[String]): Term = {
    λs("sf", λc("sf", "sf") apply 5) apply 0
  }

}
