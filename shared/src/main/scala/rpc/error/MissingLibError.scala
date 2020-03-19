package rpc.error

import rpc.LibInt

case class MissingLibError(name: String)
    extends Exception(
      s"$name is not a library function in ${(rpc.Lib: LibInt).functions.keys.mkString}")
