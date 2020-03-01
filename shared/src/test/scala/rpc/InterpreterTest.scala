package rpc

import cats.effect.IO
import rpc.InterTerm.{ClosedLam, LamRef, LamStore}
import rpc.Interpreter._
import rpc.Location.Client
import rpc.Value.Closure

import scala.collection.mutable

class InterpreterTest extends org.scalatest.FunSuite {
  import Dsl._

  test("Interpreter should add a function to the function store") {
    val id           = λˢ("test", "test")
    val (ref, store) = InterTerm.compileForInterpreter(Infer.infer(id))

    assert(
      store(InterTerm.LamRef(0, Location.Server)) === ClosedLam(
        0,
        InterTerm.Var(0),
        InterTerm.Var(0),
        Nil))
    assert(ref === InterTerm.LamRef(0, Location.Server))
  }

  test("Interpreter should have unique ids for function store") {
    val identityToConst = λc("f", "f".v apply 5) apply λc("x", "x")
    val inferred        = Infer.infer(identityToConst)
    val (term, store)   = InterTerm.compileForInterpreter(inferred)

    assert(store.keys === Set(LamRef(0, Client), LamRef(1, Client)))
  }

  val testEff = IO.pure(Right(Value.Const(999): Value): Either[CallInfo, Value])
  test("Interpreter should interpret client side function completely") {
    val identityToConst = λc("f", "f".v apply 5) apply λc("x", "x")
    val inferred        = Infer.infer(identityToConst)
    val (term, store)   = InterTerm.compileForInterpreter(inferred)
    val stackTestResult = Interpreter.runClient(term, store, Map.empty) { _ =>
      testEff
    }(_ => ???)
    assert(stackTestResult.unsafeRunSync() === Value.Const(5))
  }

  test("Interpreter should create a server deferral for server requests") {
    val identityToConst = λc("f", "f".v apply 5) apply λs("x", "x")

    val inferred      = Infer.infer(identityToConst)
    val (term, store) = InterTerm.compileForInterpreter(inferred)
    val stackTestResult = Interpreter.runClient(term, store, Map.empty) { x =>
      assert(
        x === CallInfo(LamRef(1, Location.Server), Value.Const(5), Seq.empty))
      testEff
    }(_ => ???)
    assert(stackTestResult.unsafeRunSync() === Value.Const(999))
  }

  test("Interpreter should continue after a server response") {
    val identityToConst =
      λc("cid", "cid".v) apply (λs("id", "id") apply λc("test", "test"))

    val inferred      = Infer.infer(identityToConst)
    val (term, store) = InterTerm.compileForInterpreter(inferred)

    var usedServer = false
    val stackTestResult = Interpreter.runClient(term, store, Map.empty) { x =>
      IO {
        usedServer = true
        Right(x.bound): Either[CallInfo, Value]
      }
    }(_ => ???)
    assert(
      stackTestResult.unsafeRunSync() === Closure(LamRef(2, Location.Client),
                                                  Map.empty))
    assert(usedServer)
  }

  // TODO: test free variables
  // TODO: test case where free values with the same name overwrite outside the closure

  test("Interpreter should continue after an actual server response") {
    val identityToConst =
      λc("cid", "cid".v) apply (λs("id", "id") apply λc("test", "test"))

    val stackTestResult = fullRun(identityToConst)

    assert(
      stackTestResult.unsafeRunSync() === Closure(LamRef(2, Location.Client),
                                                  Map.empty))
  }

  def fullRun(term: Term): IO[Value] = {
    val inferred           = Infer.infer(term)
    val (interTerm, store) = InterTerm.compileForInterpreter(inferred)
    val q                  = mutable.Queue[Cont[Value]]()

    def qExternal(either: Either[ExternalCall, Value]) = {
      val cinfo = for {
        test <- either.swap
      } yield {
        q.enqueue(test.cont)
        test.callInfo
      }
      cinfo.swap
    }

    Interpreter.runClient[IO](interTerm, store, Map.empty) { callInfo =>
      IO(qExternal(Interpreter.performServerRequest(store, callInfo)))
    } { value =>
      IO(qExternal(Interpreter.handleClientResponse[IO](store, value, q)))
    }
  }

  test("Interpreter should reply to simple server requests") {
    val callToClient = λs("sf", λc("sf", "sf") apply "sf") apply 5
    assert(fullRun(callToClient).unsafeRunSync() === Value.Const(5))
  }

  test("Interpreter should properly deal with shadowed variables") {
    val callToClient = λs("sf", λc("sf", "sf") apply 5) apply 0
    assert(fullRun(callToClient).unsafeRunSync() === Value.Const(5))
  }
}
