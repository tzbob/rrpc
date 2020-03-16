package rpc

import cats.syntax.functor._
import io.circe.{Decoder, DecodingFailure, HCursor}

sealed trait Literal
object Literal {
  case class Int(i: scala.Int)           extends Literal
  case class String(s: java.lang.String) extends Literal
  case class Bool(b: Boolean)            extends Literal
  case object Unit                       extends Literal

  implicit val litIntD: Decoder[Int] = (c: HCursor) =>
    c.downField("IntLit").as[scala.Int].map(Int.apply)
  implicit val litStringD: Decoder[String] = (c: HCursor) =>
    c.downField("StrLit").as[java.lang.String].map(String.apply)
  implicit val litBoolD: Decoder[Bool] = (c: HCursor) =>
    c.downField("BoolLit").as[Boolean].map(Bool.apply)
  implicit val litUnitD: Decoder[Literal.Unit.type] = (c: HCursor) => {
    if (c.downField("UnitLit").succeeded)
      Left(DecodingFailure(s"Cannot decode $c into UnitLit", c.history))
    else Right(Literal.Unit)
  }

  implicit val litD: Decoder[Literal] =
    List[Decoder[Literal]](litBoolD.widen,
                           litIntD.widen,
                           litStringD.widen,
                           litUnitD.widen).reduceLeft(_ or _)
}
