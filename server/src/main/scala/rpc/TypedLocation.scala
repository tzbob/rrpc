package rpc

sealed trait TypedLocation {
  val superscript: String
}

object TypedLocation {
  case class Var(id: Int) extends TypedLocation {
    val superscript =s"ˣ$id"
    override def toString: String = s"TLVar$id"
  }
  case class Location(loc: rpc.Location) extends TypedLocation {
    val superscript = loc.superscript
  }
}
