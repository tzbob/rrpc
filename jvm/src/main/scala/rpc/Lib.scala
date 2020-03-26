package rpc

object Lib extends LibInt {
  val nativeFunctions: Map[String, (List[Tpe], List[Value] => Value)] = Map()
}
