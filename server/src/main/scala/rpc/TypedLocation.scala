package rpc

import rpc.Location.{Client, Server}

sealed trait TypedLocation {
  val superscript: String
}

object TypedLocation {
  case class Var(id: Int) extends TypedLocation {
    val superscript = s"ˣ$id"
  }
  case class Location(loc: rpc.Location) extends TypedLocation {
    val superscript = loc.superscript
  }

  val client = Location(Client)
  val server = Location(Server)
}
