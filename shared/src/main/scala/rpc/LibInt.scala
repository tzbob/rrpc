package rpc

trait LibInt {
  val sharedFunctions: Map[String, List[Value] => Value] = Map(
    ("print", {
      case v :: _ =>
        println(v)
        Value.Constant(Literal.Unit)
    }),
  )
  val nativeFunctions: Map[String, List[Value] => Value]

  def functions: Map[String, List[Value] => Value] =
    sharedFunctions ++ nativeFunctions

  def get(name: String): Option[List[Value] => Value] = functions.get(name)
}
