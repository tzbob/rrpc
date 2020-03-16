package rpc

import cats.effect.IO
import rpc.Expr.Closed
import rpc.Expr.Closed.{ClosedLam, LamRef}
import rpc.Expr.Open._
import rpc.Interpreter._
import rpc.Value.Closure

class InterpreterTest extends org.scalatest.FunSuite {
  import Dsl._

  val tInt = Tpe.Data("Int", Nil)

  test("Interpreter should add a function to the function store") {
    val id           = λˢ('t', tInt)('t')
    val (ref, store) = Closed.compileForInterpreter(id)

    assert(
      store(Closed.LamRef(0, s)) === ClosedLam(0,
                                               Closed.Var("t"),
                                               Closed.Var("t"),
                                               Nil))
    assert(ref === Closed.LamRef(0, s))
  }

  test("Interpreter should have unique ids for function store") {
    val identityToConst = λc('f', tInt)('f'.v(Lit(5), c))(λc('x', tInt)('x'), c)
    val (term, store)   = Closed.compileForInterpreter(identityToConst)

    assert(store.keys === Set(LamRef(0, c), LamRef(1, c)))
  }

  val testEff =
    IO.pure(Right(Value.Constant(999): Value): Either[CallInfo, Value])
  test("Interpreter should interpret client side function completely") {
    val identityToConst = λc('f', tInt)('f'.v(Lit(5), c)) apply (λc('x', tInt)(
      'x'), c)
    val (term, store) = Closed.compileForInterpreter(identityToConst)
    val stackTestResult = Interpreter.runClient(term, store, Map.empty) { _ =>
      testEff
    }(_ => ???)
    assert(stackTestResult.unsafeRunSync() === Value.Constant(5))
  }

  test("Interpreter should create a server deferral for server requests") {
    val identityToConst = λc('f', tInt)('f'.v apply (Lit(5), s)) apply (λs(
      'x',
      tInt)('x'), c)

    val (term, store) = Closed.compileForInterpreter(identityToConst)
    val stackTestResult = Interpreter.runClient(term, store, Map.empty) { x =>
      assert(x === CallInfo(LamRef(1, s), Value.Constant(5), Seq.empty))
      testEff
    }(_ => ???)
    assert(stackTestResult.unsafeRunSync() === Value.Constant(999))
  }

  test("Interpreter should continue after a server response") {
    val identityToConst =
      λc('c', tInt)('c'.v) apply (λs('i', tInt)('i') apply (λc('t', tInt)('t'), s), c)

    val (term, store) = Closed.compileForInterpreter(identityToConst)

    var usedServer = false
    val stackTestResult = Interpreter.runClient(term, store, Map.empty) { x =>
      IO {
        usedServer = true
        Right(x.bound): Either[CallInfo, Value]
      }
    }(_ => ???)
    assert(
      stackTestResult.unsafeRunSync() === Closure(Right(LamRef(2, c)),
                                                  Map.empty))
    assert(usedServer)
  }

  // TODO: test free variables
  // TODO: test case where free values with the same name overwrite outside the closure

  test("Interpreter should continue after an actual server response") {
    val identityToConst =
      λc('c', tInt)('c'.v) apply ((λs('i', tInt)('i') apply (λc('t', tInt)('t'), s)), c)

    val stackTestResult = TestRunner.fullRun(identityToConst)

    assert(
      stackTestResult.unsafeRunSync() === Closure(Right(LamRef(2, c)),
                                                  Map.empty))
  }
  test("Interpreter should reply to simple server requests") {
    val callToClient = λs('s', tInt)(λc('s', tInt)('s') apply ('s', s)) apply (Lit(
      5), s)
    assert(
      TestRunner.fullRun(callToClient).unsafeRunSync() === Value.Constant(5))
  }

  test("Interpreter should properly deal with shadowed variables") {
    val callToClient = λs('s', tInt)(λc('s', tInt)('s') apply (Lit(5), s)) apply (Lit(
      0), s)
    assert(
      TestRunner.fullRun(callToClient).unsafeRunSync() === Value.Constant(5))
  }
}
