package rpc

import io.circe.generic.JsonCodec
import rpc.Expr.Closed

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
                     values: Map[String, Value]) {
    def add(k: String, value: Value): Minimal =
      copy(values = values + (k -> value))
    override def toString: String = {
      s"Env.Minimal($tpes, $locs, ${values.keys}(values omitted))"
    }
    def toEnv: Env = Env(tpes, locs, values)
  }
  object Minimal { val empty: Minimal = Env.minimize(Env.empty, Nil, Nil, Nil) }

  def minimize(source: Env,
               tpes: List[Tpe.Var],
               locs: List[Location.Var],
               values: List[Closed.Var]): Minimal = {
    Minimal(
      source.tpes.view.filterKeys(tpes.map(_.str) contains _).toMap,
      source.locs.view.filterKeys(locs.map(_.name) contains _).toMap,
      source.values.view.filterKeys(values.map(_.name) contains _).toMap
    )
  }
}
