package rpc

import Expr.Open._

object Dsl {

  /**
    * Every Term can be applied to another to create an App term
    * @param term
    */
  implicit class TermApp(term: Expr) {
    def apply(param: Expr, loc: Location): App = App(term, param, Some(loc))
  }

  // Syntax for 's.v and automatic forming of terms of symbols where possible
  implicit class SymboDsl(symbol: Char) { val v = symbToVar(symbol) }
  implicit def symbToVar(symbol: Char): Var = Var(symbol.toString)

  // Automatic forming of constant terms
  implicit def intLit(int: Int): Literal    = Literal.Int(int)
  implicit def strLit(s: String): Literal   = Literal.String(s)
  implicit def unitLit(u: Unit): Literal    = Literal.Unit
  implicit def boolLit(b: Boolean): Literal = Literal.Bool(b)

  implicit def literalToLit(l: Literal): Lit = Lit(l)

  // Sugar for client/server lambdas
  def λˢ(name: Char, tpe: Tpe)(body: Expr): Abs = Ls(name, tpe)(body)
  def λs(name: Char, tpe: Tpe)(body: Expr): Abs = Ls(name, tpe)(body)
  def Ls(name: Char, tpe: Tpe)(body: Expr): Abs =
    Abs(List((name.toString, tpe, Location.server)), body)

  def λᶜ(name: Char, tpe: Tpe)(body: Expr): Abs = Lc(name, tpe)(body)
  def λc(name: Char, tpe: Tpe)(body: Expr): Abs = Lc(name, tpe)(body)
  def Lc(name: Char, tpe: Tpe)(body: Expr): Abs =
    Abs(List((name.toString, tpe, Location.client)), body)

  val c = Location.client
  val s = Location.server
}
