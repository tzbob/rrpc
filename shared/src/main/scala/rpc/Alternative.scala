package rpc

import io.circe.generic.JsonCodec
import io.circe.{Decoder, HCursor}

case class Alternative[E](name: String, params: List[String], expr: E)
object Alternative {
  implicit val alternativeD: Decoder[Alternative[Expr.Open.Expr]] =
    (c: HCursor) =>
      c.as[(String, List[String], Expr.Open.Expr)].map {
        case (str, strs, expr) =>
          Alternative(str, strs, expr)
    }
}
