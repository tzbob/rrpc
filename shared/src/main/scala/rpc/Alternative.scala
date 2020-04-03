package rpc

trait Alternative[E]

object Alternative {
  case class Alt[E](name: String, params: List[String], expr: E)
      extends Alternative[E]
  case class TupAlt[E](params: List[String], expr: E) extends Alternative[E]
}
