package rpc

import rpc.InterTerm.LamRef

sealed trait Value

object Value {
  case class Const(i: Int) extends Value
  case class Closure(lamRef: LamRef, env: Map[InterTerm.Var, Value])
      extends Value
}
