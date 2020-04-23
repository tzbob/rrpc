package rpc

import rpc.Declaration.TopLevel
import rpc.Expr.Open

case class AppData(name: String,
                   raw: String,
                   decoded: List[TopLevel[Open.Expr]],
                   headContent: String,
                   bodyContent: String)
