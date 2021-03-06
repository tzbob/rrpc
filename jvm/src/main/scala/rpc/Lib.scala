package rpc

import rpc.Expr.Open

import scala.io.StdIn

object Lib extends LibInt {
  import Dsl._
  import Open._

  val nativeFunctions: Map[String, LibData[Open.Expr]] = Map(
    ("read",
     LibData(
       LocAbs(List("l"),
              Abs(List(("x", UnitTpe, 'l')), Native("read", List()))), {
         (_: List[Value]) =>
           val str = StdIn.readLine()
           Value.Constant(Literal.String(str))
       }
     ))
  )
}
