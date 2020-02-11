package rpc

import Term._

object RPC {

  def eval(term: Term, location: Location): Term.Value = term match {
    case Lam(loc, name, body) => term
    case App(lam, p) =>
      val Lam(lamLoc, name, body) = eval(lam, location)
      val evaledP = eval(p, location)
      eval(substitute(body, name, evaledP), lamLoc)
    case Const(i) => term
  }

  def substitute(term: Term, name: String, value: Value): Term = term match {
    case Var(v) => if (v == name) value else term
    case Lam(loc, n, body) =>
      if (n == name) term // when substituting the current variable
      else Lam(loc, n, substitute(body, name, value))
    case App(fun, param) =>
      App(substitute(fun, name, value), substitute(param, name, value))
    case Const(i) => term
  }

}
