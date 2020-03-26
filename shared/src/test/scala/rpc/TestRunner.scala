package rpc

import cats.effect.IO
import rpc.Expr.Closed.LamStore
import rpc.Expr.{Closed, Open}
import rpc.Interpreter._

import scala.collection.mutable

object TestRunner {
  def fullRun(term: Open.Expr): IO[Value] = {
    val (interTerm, store) = Closed.compileForInterpreter(term, LamStore.empty)
    val (cf, vf)           = fullRunIOFunctions(store)
    Interpreter.runClient[IO](interTerm, Env.empty)(cf)(vf)
  }

  def fullRunIOFunctions(store: LamStore) = {
    val q = mutable.Queue[Cont[Value]]()

    def qExternal(either: Either[ExternalCall, Value]) = {
      val cinfo = for {
        test <- either.swap
      } yield {
        q.enqueue(test.cont)
        test.callInfo
      }
      cinfo.swap
    }

    val callInfoF = (callInfo: CallInfo) =>
      IO(qExternal(Interpreter.performServerRequest(store, callInfo)))
    val valueF = (value: Value) =>
      IO(qExternal(Interpreter.handleClientResponse[IO](store, value, q)))
    (callInfoF, valueF)
  }

}
