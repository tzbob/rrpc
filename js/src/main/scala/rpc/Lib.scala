package rpc

import org.scalajs.dom
import rpc.Expr.Open

object Lib extends LibInt {
  import Dsl._
  import Open._
  val nativeFunctions: Map[String, LibData[Open.Expr]] = Map(
    ("read",
     LibData(
       LocAbs(List("l"),
              Abs(List(("x", UnitTpe, 'l')), Native("read", List()))), {
         (_: List[Value]) =>
           val str = dom.window.prompt()
           Value.Constant(Literal.String(str))
       }
     ))
  )
}
