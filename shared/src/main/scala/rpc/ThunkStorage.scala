package rpc

import scala.collection.mutable.ArrayBuffer

object ThunkStorage {
  private val storage = ArrayBuffer.empty[Option[Value]]

  def init(): Int = this.synchronized {
    storage.addOne(None)
    storage.size - 1
  }

  def read(address: Int): Option[Value] = this.synchronized {
    storage(address)
  }
  def write(address: Int, value: Value) = this.synchronized {
    storage(address) = Some(value)
  }
}
