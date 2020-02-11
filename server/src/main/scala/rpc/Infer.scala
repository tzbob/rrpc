package rpc

object Infer {

  def infer(term: Term): TypedTerm = {
    val constraints = generateConstraints(0, term, Map.empty)
    val tpeUni      = Equation.unifyTpes(constraints.tpeEqs.toList)
    val locSub      = Equation.unifyLocs(tpeUni.locAcc ++ constraints.locEqs.toList)
    TypedTerm.substitute(constraints.typedTerm, tpeUni.tpeSub.toMap, locSub)
  }

  type TpeEnv = Map[String, Tpe]
  case class ConstraintResults(typedTerm: TypedTerm,
                               tpe: Tpe,
                               tpeEqs: Set[(Tpe, Tpe)],
                               locEqs: Set[(TypedLocation, TypedLocation)],
                               id: Int)

  def generateConstraints(id: Int, term: Term, env: TpeEnv): ConstraintResults =
    term match {
      case t @ Term.Const(_) =>
        ConstraintResults(t.typed, Tpe.Int, Set.empty, Set.empty, id)

      case t @ Term.Var(name) =>
        ConstraintResults(t.typed, env(name), Set.empty, Set.empty, id)

      case Term.App(fun, param) =>
        val fResult = generateConstraints(id, fun, env)
        val pResult = generateConstraints(fResult.id, param, env)

        // introduce type location variables to build function type
        val locTpe = TypedLocation.Var(pResult.id)
        // type of function application (result type)
        val returnTpe = Tpe.Var(pResult.id + 1)

        // typed app term (a.apply_loc(b) with a and b typed terms
        val tt = TypedTerm.App(locTpe, fResult.typedTerm, pResult.typedTerm)

        // we know that the function has to be of type paramType =loc=> returnType
        val funEq = (fResult.tpe, Tpe.Fun(pResult.tpe, locTpe, returnTpe))

        ConstraintResults(tt,
                          returnTpe,
                          fResult.tpeEqs ++ (pResult.tpeEqs + funEq),
                          fResult.locEqs ++ pResult.locEqs,
                          pResult.id + 2)

      case Term.Lam(loc, name, body) =>
        val argTpe = Tpe.Var(id)
        // introduce new variable into the store
        val expandedEnv = env + (name -> argTpe)
        // type body under new environment
        val bResult = generateConstraints(id + 1, body, expandedEnv)

        val funTpe = Tpe.Fun(argTpe, TypedLocation.Location(loc), bResult.tpe)
        val tt     = TypedTerm.Lam(loc, name, argTpe, bResult.typedTerm)

        ConstraintResults(tt,
                          funTpe,
                          bResult.tpeEqs,
                          bResult.locEqs,
                          bResult.id)
    }
  /*

5) Propagate process
- Left-hand type of each equation has different variables on the right-hand side of the equation
Replace with the right-hand side that matches the left-hand type variable when it appears

a1 = int -c-> int
l5 = c
a4 = int
a2 = int
l3 = c
In this example, you don't have to repeat it anymore.
In general, 3)~5) As a result of a new equation created during the process,
There are instances where it has to be repeated.
6) Equation with substTerm shall be prepared in the genCst step.
Replace typed term with skeletons and replace it with complete typed term.
To complete.
-}
 */

}
