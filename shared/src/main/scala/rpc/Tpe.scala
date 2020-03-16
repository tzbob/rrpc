package rpc

import cats.syntax.functor._
import io.circe.{Decoder, HCursor}

sealed trait Tpe
object Tpe {
  case class Var(str: String)                                  extends Tpe
  case class Tup(tpes: List[Tpe])                              extends Tpe
  case class Fun(a: Tpe, loc: Location, b: Tpe)                extends Tpe
  case class TypeAbs(abstractions: List[String], tpeBody: Tpe) extends Tpe
  case class LocAbs(abstractions: List[String], tpeBody: Tpe)  extends Tpe
  case class Data(name: String, fields: List[Tpe])             extends Tpe

  private implicit val prioritizedLocationDecoder: Decoder[Location] = Location.locD

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
    c.downField("ConType").as[(String, List[Tpe])].map(Data.tupled)

  implicit val tpeD: Decoder[Tpe] =
    List[Decoder[Tpe]](tpeVarD.widen,
                       tpeTupleD.widen,
                       tpeFunD.widen,
                       tpeAbsD.widen,
                       tpeLocAbsD.widen,
                       tpeDataD.widen).reduceLeft(_ or _)
}

//sealed trait Tpe
//
//object Tpe {
//  case object Int                                    extends Tpe
//  case class Var(id: Int)                            extends Tpe
//  case class Fun(a: Tpe, loc: TypedLocation, b: Tpe) extends Tpe
//
//
//  def all(tpe: Tpe): LazyList[Tpe] =
//    tpe #:: (tpe match {
//      case Fun(a, loc, b) =>
//        all(a) #::: all(b)
//      case x => LazyList(x)
//    })
//
//  implicit object PrettyTpe extends Show[Tpe] {
//    override def show(t: Tpe): String = t match {
//      case Int            => "Int"
//      case Var(id)        => s"Var$id"
//      case Fun(a, loc, b) => s"${show(a)} ->${loc.superscript} ${show(b)}"
//    }
//  }
//}
