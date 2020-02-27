package rpc

import cats.effect.IO
import rpc.InterTerm.{ClosedLam, LamRef}
import rpc.Interpreter._
import rpc.Location.Client

class InterpreterTest extends org.scalatest.FunSuite {
  import Dsl._

  test("Interpreter should add a function to the function store") {
    val id           = λˢ('test, 'test)
    val (ref, store) = InterTerm.compileForInterpreter(Infer.infer(id))

    assert(
      store(InterTerm.LamRef(0, Location.Server)) === ClosedLam(
        0,
        InterTerm.Var(0, "test"),
        InterTerm.Var(0, "test"),
        Nil))
    assert(ref === InterTerm.LamRef(0, Location.Server))
  }

  test("Interpreter should have unique ids for function store") {
    val identityToConst = λc('f, 'f.v apply 5) apply λc('x, 'x)
    val inferred        = Infer.infer(identityToConst)
    val (term, store)   = InterTerm.compileForInterpreter(inferred)

    assert(store.keys === Set(LamRef(0, Client), LamRef(1, Client)))
  }

  val testEff = IO.pure(Value.Const(999): Value)
  test("Interpreter should interpret client side function completely") {
    val identityToConst = λc('f, 'f.v apply 5) apply λc('x, 'x)
    val stackTestResult = Interpreter.runClient(identityToConst) { _ =>
      testEff
    }
    assert(stackTestResult.unsafeRunSync() === Value.Const(5))
  }

  test("Interpreter should create a server deferral for server requests") {
    val identityToConst = λc('f, 'f.v apply 5) apply λs('x, 'x)
    val stackTestResult = Interpreter.runClient(identityToConst) { x =>
      assert(
        x === CallInfo(LamRef(1, Location.Server), Value.Const(5), Seq.empty))
      testEff
    }
    assert(stackTestResult.unsafeRunSync() === Value.Const(999))
  }
//
//  test("Interpreter should continue after a server response") {
//    val identityToConst =
//      λc('cid, 'cid.v) apply (λs('id, 'id) apply λc('test, 'test))
//
//    val inferred      = Infer.infer(identityToConst)
//    val (term, store) = Interpreter.compileForInterpreter(inferred)
//
//    var usedServer = false
//    val stackTestResult = Interpreter.runClient(term, store) { x =>
//      IO {
//        usedServer = true
//        x.variables.head // only 1 value and the server is the id function
//      }
//    }
//    assert(
//      stackTestResult.unsafeRunSync() === InterTerm.LamRef(2, Location.Client))
//    assert(usedServer)
//  }
//
//  // TODO: test free variables
//  // TODO: test case where free values with the same name overwrite outside the closure
//
//  test("Interpreter should continue after an actual server response") {
//    val identityToConst =
//      λc('cid, 'cid.v) apply (λs('id, 'id) apply λc('test, 'test))
//
//    val inferred      = Infer.infer(identityToConst)
//    val (term, store) = Interpreter.compileForInterpreter(inferred)
//
//    val stackTestResult =
//      Interpreter.runClient[IO](term, store)(Interpreter.requestResponder[IO](store))
//
//    assert(
//      stackTestResult.unsafeRunSync() === InterTerm.LamRef(2, Location.Client))
//  }
}
