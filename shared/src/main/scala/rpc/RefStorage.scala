package rpc

import scala.collection.mutable.ArrayBuffer

object RefStorage {
  private val storage = ArrayBuffer.empty[Value]

  def init(value: Value): Int = this.synchronized {
    storage.addOne(value)
    storage.size - 1
  }

  def read(address: Int): Value = this.synchronized {
    storage(address)
  }
  def write(address: Int, value: Value) = this.synchronized {
    storage(address) = value
  }
}
