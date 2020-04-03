package rpc

import rpc.Expr.Open

object Lib extends LibInt {
  val nativeFunctions: Map[String, LibData[Open.Expr]] = Map()
}
