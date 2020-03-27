package rpc

import cats.syntax.functor._
import io.circe.generic.JsonCodec
import io.circe.{Decoder, HCursor}

trait Alternative[E]

object Alternative {

  case class Alt[E](name: String, params: List[String], expr: E)
      extends Alternative[E]
  case class TupAlt[E](params: List[String], expr: E) extends Alternative[E]

  private implicit val altD: Decoder[Alt[Expr.Open.Expr]] =
    (c: HCursor) =>
      c.downField("Alternative").as[(String, List[String], Expr.Open.Expr)].map {
        case (str, strs, expr) => Alt(str, strs, expr)
    }

  private implicit val tupleAltD: Decoder[TupAlt[Expr.Open.Expr]] =
    (c: HCursor) =>
      c.downField("TupleAlternative").as[(List[String], Expr.Open.Expr)].map {
        case (strs, expr) => TupAlt(strs, expr)
    }

  implicit val alternativeD: Decoder[Alternative[Expr.Open.Expr]] =
    List[Decoder[Alternative[Expr.Open.Expr]]](
      altD.widen,
      tupleAltD.widen
    ).reduceLeft(_ or _)
}
