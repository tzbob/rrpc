package rpc

import java.util.concurrent.atomic.AtomicInteger

object IdCreator {
  private val count = new AtomicInteger(-1)
  def get() = count.getAndIncrement()
}
