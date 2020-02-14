package rpc

trait StateTerm

object StateTerm {
  case class Const(i: Int)     extends StateTerm
  case class Var(name: String) extends StateTerm
  case class Lam(location: TypedLocation, name: List[String], body: StateTerm)
      extends StateTerm
  case class App(loc: TypedLocation, fun: StateTerm, param: StateTerm)
      extends StateTerm

  case class Let(name: String, value: StateTerm, body: StateTerm)
      extends StateTerm

  // Tiered terms
  case class Return(value: StateTerm)                       extends StateTerm
  case class Call(fun: StateTerm, args: List[StateTerm])    extends StateTerm
  case class Request(fun: StateTerm, args: List[StateTerm]) extends StateTerm
}
