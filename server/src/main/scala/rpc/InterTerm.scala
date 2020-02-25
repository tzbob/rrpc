package rpc

sealed trait InterTerm

object InterTerm {
  case class Const(i: Int)     extends InterTerm
  case class Var(name: String) extends InterTerm
  object Var {
    def fromTVar(typedTerm: TypedTerm.Var): Var = Var(typedTerm.name)
  }
  case class LamRef(id: Int, loc: Location) extends InterTerm
  case class App(loc: TypedLocation, fun: InterTerm, param: InterTerm)
      extends InterTerm

  case class ClosedLam(id: Int,
                       body: InterTerm,
                       boundedVar: InterTerm.Var,
                       freeVars: Seq[InterTerm.Var])

}
