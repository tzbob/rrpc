package rpc

import rpc.Expr.Closed.{LamRef, LamStore}

object LamStore {
  def empty: LamStore = Map.empty
  def newRef(lamStore: LamStore): LamRef = {
    val ids = lamStore.keys.map(_.id)
    LamRef(ids.toList.maxOption.map(_ + 1).getOrElse(0))
  }
}
