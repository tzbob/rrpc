package rpc

import io.circe.generic.JsonCodec
import rpc.Expr.Closed
import rpc.Expr.Closed.LamStore
import io.circe.generic.auto._

@JsonCodec
case class Env(tpes: Map[String, Tpe],
               locs: Map[String, Location],
               values: Map[String, Value]) {
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

  def location(k: String): Option[Location] = locs.get(k)
  def tpe(k: String): Option[Tpe]           = tpes.get(k)
  def value(k: String): Option[Value]       = values.get(k)
}

object Env {
  val empty: Env = Env(Map.empty, Map.empty, Map.empty)
  case class Minimal(tpes: Map[String, Tpe],
                     locs: Map[String, Location],
                     values: Map[String, Value])
}
