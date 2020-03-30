package rpc

import cats.syntax.functor._
import io.circe.generic.JsonCodec
import io.circe.{Decoder, HCursor}

sealed trait Tpe
object Tpe {
  case class Var(str: String)                                  extends Tpe
  case class Tup(tpes: List[Tpe])                              extends Tpe
  case class Fun(a: Tpe, loc: Location, b: Tpe)                extends Tpe
  case class TypeAbs(abstractions: List[String], tpeBody: Tpe) extends Tpe
  case class LocAbs(abstractions: List[String], tpeBody: Tpe)  extends Tpe
  case class Data(name: String, locs: List[Location], fields: List[Tpe])
      extends Tpe

  private implicit val prioritizedLocationDecoder: Decoder[Location] =
    Location.locD

  private implicit val tpeVarD: Decoder[Var] = (c: HCursor) =>
    c.downField("TypeVarType").as[String].map(Var)
  private implicit val tpeTupleD: Decoder[Tup] = (c: HCursor) =>
    c.downField("TupleType").as[List[Tpe]].map(Tup.apply)
  private implicit val tpeFunD: Decoder[Fun] = (c: HCursor) =>
    c.downField("FunType").as[(Tpe, Location, Tpe)].map(Fun.tupled)
  private implicit val tpeAbsD: Decoder[TypeAbs] = (c: HCursor) =>
    c.downField("TypeAbsType").as[(List[String], Tpe)].map(TypeAbs.tupled)
  private implicit val tpeLocAbsD: Decoder[LocAbs] = (c: HCursor) =>
    c.downField("LocAbsType").as[(List[String], Tpe)].map(LocAbs.tupled)
  private implicit val tpeDataD: Decoder[Data] = (c: HCursor) =>
    c.downField("ConType")
      .as[(String, List[Location], List[Tpe])]
      .map(Data.tupled)

  implicit val tpeD: Decoder[Tpe] =
    List[Decoder[Tpe]](tpeVarD.widen,
                       tpeTupleD.widen,
                       tpeFunD.widen,
                       tpeAbsD.widen,
                       tpeDataD.widen,
                       tpeLocAbsD.widen).reduceLeft(_ or _)
}
