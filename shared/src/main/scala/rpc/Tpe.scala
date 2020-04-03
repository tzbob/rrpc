package rpc

import io.circe.generic.JsonCodec

@JsonCodec sealed trait Tpe
object Tpe {
  case class Var(str: String)                                  extends Tpe
  case class Tup(tpes: List[Tpe])                              extends Tpe
  case class Fun(a: Tpe, loc: Location, b: Tpe)                extends Tpe
  case class TypeAbs(abstractions: List[String], tpeBody: Tpe) extends Tpe
  case class LocAbs(abstractions: List[String], tpeBody: Tpe)  extends Tpe
  case class Data(name: String, locs: List[Location], fields: List[Tpe])
      extends Tpe
}
