package rpc.error

import rpc.Value

case class MissingValueError(name: String, values: Map[String, Value])
    extends Exception(s"$name is not a value in [${values.keys.mkString(", ")}]")
