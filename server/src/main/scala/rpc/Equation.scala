package rpc

import rpc.Infer.ConstraintResults

object Equation {
  type TpeEq = (Tpe, Tpe)
  type LocEq = (TypedLocation, TypedLocation)

  case class UnifyError(tup: (Any, Any))
      extends RuntimeException(s"Cannot unify ${tup._1} and ${tup._2}")

  case class TpeUnifyResult(tpeSub: List[TpeEq], locAcc: List[LocEq])

  def substitute[A](target: A,
                    replacement: A,
                    list: List[(A, A)]): List[(A, A)] = {
    list.map {
      case (l, r) =>
        (if (l == target) replacement else l,
         if (r == target) replacement else r)
    }
  }

  //  https://papl.cs.brown.edu/2016/Type_Inference.html
  def unifyTpes(tpeEqs: List[TpeEq]): TpeUnifyResult = {
    val T = rpc.Tpe

    /**
      * eliminate from tpeEqs by recording in substitution
      */
    def helper(tpeEqs: List[TpeEq], acc: TpeUnifyResult): TpeUnifyResult = {
      tpeEqs match {
        case Nil => acc
        case tpeEq :: tpeEqsRest =>
          tpeEq match {
            case (l @ T.Var(_), r) =>
              // substitution can create more equations in acc
              val roughTpeSub = substitute(l, r, acc.tpeSub)
              val (stableSub, newEqs) = roughTpeSub.partition {
                case (T.Var(_), _) => true
                case _             => false
              }
              helper(substitute(l, r, tpeEqsRest) ++ newEqs,
                     acc.copy(tpeSub = tpeEq :: stableSub))

            // trivial
            case (T.Int, T.Int) => helper(tpeEqsRest, acc)

            // swap variable to left hand side
            case (l @ T.Int, r @ T.Var(_)) =>
              helper((r, l) :: tpeEqsRest, acc)
            case (l @ T.Fun(_, _, _), r @ T.Var(_)) =>
              helper((l, r) :: tpeEqsRest, acc)
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
              helper(substitute(l, r, locEqsRest),
                     leq :: substitute(l, r, locSub))
            case (l, r @ TypedLocation.Var(_)) =>
              helper((r, l) :: locEqsRest, locSub)
            case (_, _) => helper(locEqsRest, locSub)
          }
      }
    }

    helper(locEqs, List.empty).toMap
  }

  def unify(constraints: ConstraintResults) = {
    val tpeUni  = Equation.unifyTpes(constraints.tpeEqs.toList)
    val locSubs = Equation.unifyLocs(tpeUni.locAcc ++ constraints.locEqs.toList)
  }

}
