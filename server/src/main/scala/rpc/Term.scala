package rpc

trait Term

object Term {
  case class Const(int: Int) extends Term {
    val typed = TypedTerm.Const(int)
  }
  case class Var(name: String) extends Term {
    val typed = TypedTerm.Var(name)
  }
  case class Lam(loc: Location, name: String, body: Term) extends Term
  case class App(fun: Term, param: Term)                  extends Term
}
