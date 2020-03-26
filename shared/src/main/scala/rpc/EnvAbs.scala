package rpc

import rpc.Expr.Closed
import rpc.Expr.Closed.LamStore

case class EnvAbs(boundVars: List[Closed.Var],
                  freeVars: List[Closed.Var],
                  tpeVars: List[Tpe.Var],
                  locVars: List[Location.Var]) {
  def applyBoundedVars(env: Env, bValues: List[Closed.Expr]): (Env, EnvAbs) = {
    val newEnv = boundVars.zip(bValues).foldLeft(env) {
      case (envAcc, (k, v)) =>
        envAcc.add(k.name, v)
    }
    val removedBound = this.copy(boundVars = this.boundVars.drop(bValues.size))
    newEnv -> removedBound
  }

  def applyFreeVars(env: Env, fValues: List[Closed.Expr]): (Env, EnvAbs) = {
    val newEnv = freeVars.zip(fValues).foldLeft(env) {
      case (envAcc, (k, v)) =>
        envAcc.add(k.name, v)
    }
    val removedBound = this.copy(freeVars = this.freeVars.drop(fValues.size))
    newEnv -> removedBound
  }

  def applyTpeVars(env: Env, tValues: List[Closed.Expr]): (Env, EnvAbs) = {
    val newEnv = tpeVars.zip(tValues).foldLeft(env) {
      case (envAcc, (k, v)) =>
        envAcc.add(k.str, v)
    }
    val removedBound = this.copy(tpeVars = this.tpeVars.drop(tValues.size))
    newEnv -> removedBound
  }

  def applyLocVars(env: Env, lValues: List[Closed.Expr]): (Env, EnvAbs) = {
    val newEnv = tpeVars.zip(lValues).foldLeft(env) {
      case (envAcc, (k, v)) =>
        envAcc.add(k.str, v)
    }
    val removedBound = this.copy(tpeVars = this.tpeVars.drop(lValues.size))
    newEnv -> removedBound
  }
}

object EnvAbs {}
