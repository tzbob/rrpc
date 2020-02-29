package rpc

import rpc.Infer.ConstraintResults

object Equation {
  // Equations are just tuples
  type TpeEq = (Tpe, Tpe)
  type LocEq = (TypedLocation, TypedLocation)

  /**
    * @return simple substitution of elements in a tupled list
    */
  def swap[A](target: A, replacement: A, list: List[(A, A)]): List[(A, A)] = {
    list.map {
      case (l, r) =>
        (if (l == target) replacement else l,
         if (r == target) replacement else r)
    }
  }

  /**
    * Result of unifying types.
    *
    * Type unification does not just result in a substitution list (tpeSub),
    * but also creates extra constraints on locations, e.g., when unifying two
    * @param tpeSub
    * @param locAcc
    */
  case class TpeUnifyResult(tpeSub: List[TpeEq], locAcc: List[LocEq])

  /**
    * @param tpeEqs
    * @return Unified type equations (start from https://papl.cs.brown.edu/2016/Type_Inference.html)
    */
  def unifyTpes(tpeEqs: List[TpeEq]): TpeUnifyResult = {
    val T = rpc.Tpe

    /**
      * eliminate FROM tpeEqs by recording TO substitution list
      *   (while maintaining location information)
      *
      * work towards minimizing tpeEqs (in length and in structure of Types)
      *   equations with just a variable on the lhs are moved to the substitution list
      *   extra constraints regarding locations is recorded, e.g.,
      *     case (T.Fun(la, lloc, lb), T.Fun(ra, rloc, rb))
      *
      */
    def helper(tpeEqs: List[TpeEq], acc: TpeUnifyResult): TpeUnifyResult = {
      tpeEqs match {
        case Nil => acc
        case tpeEq :: tpeEqsRest =>
          tpeEq match {
            case (l @ T.Var(_), r) =>
              // occurs check (recursive types unsupported)
              if (Tpe.all(r).toSet.contains(l)) throw UnifyError(tpeEq)

              /* Required for proper propagation in unification:

              All swaps to the substitution list that result in equations that do *not*
                have variables on the lhs should be unified.

              e.g., (x0, Fun(...)) with acc: (x0, Fun(...))
              will be substituted to:
                (Fun(....), Fun(...))
              and requires unification, so add back to tpeEqs
               */
              val roughTpeSub = swap(l, r, acc.tpeSub)
              val (stableSub, newEqs) = roughTpeSub.partition {
                case (T.Var(_), _) => true
                case _             => false
              }
              helper(swap(l, r, tpeEqsRest) ++ newEqs,
                     acc.copy(tpeSub = tpeEq :: stableSub))

            // trivial
            case (T.Int, T.Int) => helper(tpeEqsRest, acc)

            // swap variable to left hand side
            case (l @ T.Int, r @ T.Var(_)) =>
              helper((r, l) :: tpeEqsRest, acc)
            case (l @ T.Fun(_, _, _), r @ T.Var(_)) =>
              helper((r, l) :: tpeEqsRest, acc)
            case (T.Fun(la, lloc, lb), T.Fun(ra, rloc, rb)) =>
              helper((la, ra) :: (lb, rb) :: tpeEqsRest,
                     acc.copy(locAcc = (lloc, rloc) :: acc.locAcc))

            case (T.Int, T.Fun(_, _, _)) =>
              throw UnifyError(tpeEq)
            case (T.Fun(_, _, _), T.Int) =>
              throw UnifyError(tpeEq)
          }
      }
    }

    helper(tpeEqs, TpeUnifyResult(List.empty, List.empty))
  }

  def unifyLocs(locEqs: List[LocEq]): Map[TypedLocation, TypedLocation] = {
    def helper(locEqs: List[LocEq], locSub: List[LocEq]): List[LocEq] = {
      locEqs match {
        case Nil => locSub
        case leq :: locEqsRest =>
          leq match {
            case (l @ TypedLocation.Var(_), r) =>
              helper(swap(l, r, locEqsRest), leq :: swap(l, r, locSub))
            case (l, r @ TypedLocation.Var(_)) =>
              helper((r, l) :: locEqsRest, locSub)
            case (_, _) => helper(locEqsRest, locSub)
          }
      }
    }

    helper(locEqs, List.empty).toMap
  }

  case class UnifyError(tup: (Any, Any))
      extends RuntimeException(s"Cannot unify ${tup._1} and ${tup._2}")
}
