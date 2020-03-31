package rpc

import scala.collection.mutable.ArrayBuffer

object RefStorage {
  private val storage = ArrayBuffer.empty[Value]

  def init(value: Value): Int = {
    storage.addOne(value)
    storage.size - 1
  }

  def read(address: Int): Value         = storage(address)
  def write(address: Int, value: Value) = storage(address) = value
}
