package rpc

import rpc.Expr.Closed
import rpc.Expr.Closed.LamStore

case class Env(tpes: Map[String, Tpe],
               locs: Map[String, Location],
               values: Map[String, Value],
               recursive: Map[String, Closed.Expr],
               lamStore: LamStore) {
  def add(k: String, tpe: Tpe): Env = copy(tpes = tpes + (k -> tpe))
  def add(k: String, loc: Location): Env = {
    val newLocs = locs.get(k) match {
      case Some(Location.Loc(_)) => locs
      case _                     => locs + (k -> loc)
    }
    copy(locs = newLocs)
  }

  def add(k: String, value: Value): Env =
    copy(values = values + (k -> value))
  def add(k: String, expr: Closed.Expr): Env =
    copy(recursive = recursive + (k -> expr))

  def value(k: String): Option[Value]         = values.get(k)
  def recurse(k: String): Option[Closed.Expr] = recursive.get(k)

  def mergeStore(ls: LamStore): Env = copy(lamStore = this.lamStore ++ ls)
}

object Env {
  val empty: Env =
    Env(Map.empty, Map.empty, Map.empty, Map.empty, LamStore.empty)
}
