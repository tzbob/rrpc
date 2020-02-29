package rpc

import io.circe.generic.JsonCodec

sealed trait InterTerm

object InterTerm {
  case class Const(i: Int) extends InterTerm

  @JsonCodec case class LamRef(id: Int, loc: Location) extends InterTerm
  @JsonCodec case class Var(idx: Int)                  extends InterTerm
  object Var {
    def fromTVar(typedTerm: TypedTerm.Var, idx: Int): Var = Var(idx)
  }
  case class App(loc: TypedLocation, fun: InterTerm, param: InterTerm)
      extends InterTerm

  case class ClosedLam(id: Int,
                       body: InterTerm,
                       boundedVar: InterTerm.Var,
                       freeVars: Seq[InterTerm.Var])

  type LamStore = Map[LamRef, InterTerm.ClosedLam]

  def compileForInterpreter(typedTerm: TypedTerm): (InterTerm, LamStore) = {
    def helper(bruijnEnv: IndexedSeq[String],
               id: Int,
               location: Location,
               typedTerm: TypedTerm): (Int, InterTerm, LamStore) = {
      def nameToVar(name: String) =
        InterTerm.Var(bruijnEnv.lastIndexOf(name))
      typedTerm match {
        case TypedTerm.Const(i)  => (id, InterTerm.Const(i), Map.empty)
        case TypedTerm.Var(name) => (id, nameToVar(name), Map.empty)
        case TypedTerm.Lam(newLoc, name, _, body) =>
          val ref = LamRef(id, newLoc)
          val (newId, newBody, lamStore) =
            helper(bruijnEnv :+ name, id + 1, newLoc, body)
          val closedLam = ClosedLam(
            id,
            newBody,
            InterTerm.Var(bruijnEnv.size),
            TypedTerm.freeVariables(typedTerm).map(x => nameToVar(x.name)))
          (newId, ref, (lamStore + (ref -> closedLam)))
        case TypedTerm.App(l, fun, param) =>
          val (newId, funC, funStore) = helper(bruijnEnv, id, location, fun)
          val (newerId, paramC, paramStore) =
            helper(bruijnEnv, newId, location, param)
          (newerId, InterTerm.App(l, funC, paramC), (funStore ++ paramStore))
      }
    }

    val (_, term, store) =
      helper(IndexedSeq.empty, 0, Location.Client, typedTerm)
    (term, store)
  }
}
