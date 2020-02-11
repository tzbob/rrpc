package rpc

trait Term

object Term {

  case class Const(int: Int) extends Term {
    val typed = TypedTerm.Const(int)

    override def toString: String = int.toString
  }
  case class Var(name: String) extends Term {
    val typed = TypedTerm.Var(name)

    override def toString: String = name
  }
  case class Lam(loc: Location, name: String, body: Term) extends Term {

    override def toString: String = s"λ${loc.superscript}$name.$body"
  }
  case class App(fun: Term, param: Term) extends Term {
    override def toString: String = s"($fun) ($param)"
  }

  type Value = Term
}
