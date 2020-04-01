package rpc.error


case class TypeError(expr: Any, expected: String)
    extends Exception(s"$expr was not of the $expected type")
