package rpc

import org.scalatest.funsuite.AnyFunSuite

class MonoInterpreterTest extends AnyFunSuite {
  import Dsl._

  //    val trace = MonoInterpreter.trace(Infer.infer((λᶜ('x, 'x) apply λˢ('y, 'y)) apply 4))
  //    println(trace.map(TypedTerm.PrettyShow.show))

  test("Identity function") {
    val constId = MonoInterpreter.interpret(λᶜ('x, 'x) apply 5)
    assert(constId === TypedTerm.Const(5))

    val idIdInt =
      MonoInterpreter.interpret((λᶜ('x, 'x) apply λˢ('y, 'y)) apply 4)
    assert(idIdInt === TypedTerm.Const(4))
  }

//  test(
//    "term from https://stackoverflow.com/questions/34140819/lambda-calculus-reduction-steps") {
//    val stacktest       = λᶜ('xyz, 'xyz) apply λᶜ('x, 'x.v apply 'x) apply λᶜ('x, 'x) apply 5
//    val stackTestResult = MonoInterpreter.interpret(stacktest)
//    assert(stackTestResult === TypedTerm.Const(5))
//  }
}
