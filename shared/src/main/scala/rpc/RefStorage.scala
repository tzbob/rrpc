package rpc

import scala.collection.mutable.ArrayBuffer

object RefStorage {
  private val storage = ArrayBuffer.empty[Value]

  def init(value: Value): Int = {
    storage.addOne(value)
    storage.size - 1
  }

  def read(address: Int): Value         = {
    pprint.log("reading" -> storage)
    storage(address)
  }
  def write(address: Int, value: Value) = {
    pprint.log(address -> value)
    storage(address) = value
  }
}
