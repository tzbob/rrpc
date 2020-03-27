package rpc.error

import rpc.{Alternative, Value}
import rpc.Expr.Closed

case class CaseError(expr: Value, alts: List[Alternative[Closed.Expr]])
    extends Exception(s"No case for $expr in [${alts.mkString(",")}]")
