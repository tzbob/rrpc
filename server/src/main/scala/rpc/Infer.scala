package rpc

object Infer {

  /**
    * 1. Generate constraints (typed term, type equations, location equations)
    * 2. Unify type equations
    * 3. Unify location equations (from 1. and the extra from 2.)
    * 4. Apply type and location substitution to 1.'s typed term to create
    * the fully typed term
    *
    * @param term
    * @return
    */
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

  /**
    * Generate all constraints from the given term,
    *   from https://github.com/kwanghoon/rpcexample
    * @param id
    * @param term
    * @param env
    * @return
    */
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
}
