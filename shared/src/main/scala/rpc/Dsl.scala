package rpc

object Dsl {

  /**
    * Every Term can be applied to another to create an App term
    * @param term
    */
  implicit class TermApp(term: Term) {
    def apply(param: Term): Term.App = Term.App(term, param)
  }

  // Syntax for 's.v and automatic forming of terms of symbols where possible
  implicit class SymboDsl(symbol: String) { val v = symbToVar(symbol) }
  implicit def symbToVar(symbol: String): Term.Var = Term.Var(symbol)

  // Automatic forming of constant terms
  implicit def intToConst(int: Int): Term.Const = Term.Const(int)

  // Sugar for client/server lambdas
  def λˢ(name: String, body: Term): Term.Lam = Ls(name, body)
  def λs(name: String, body: Term): Term.Lam = Ls(name, body)
  def Ls(name: String, body: Term): Term.Lam =
    Term.Lam(Location.Server, name, body)

  def λᶜ(name: String, body: Term): Term.Lam = Lc(name, body)
  def λc(name: String, body: Term): Term.Lam = Lc(name, body)
  def Lc(name: String, body: Term): Term.Lam =
    Term.Lam(Location.Client, name, body)
}
