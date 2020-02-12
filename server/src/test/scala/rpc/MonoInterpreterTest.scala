package rpc

import org.scalatest.funsuite.AnyFunSuite

class MonoInterpreterTest extends AnyFunSuite {
  import Dsl._

  //    val trace = MonoInterpreter.trace(Infer.infer((λᶜ('x, 'x) apply λˢ('y, 'y)) apply 4))
  //    println(trace.map(TypedTerm.PrettyShow.show))

  test("Identity function") {

    assert(false)

    val constId = MonoInterpreter.interpret(λᶜ('x, 'x) apply 5)
    assert(constId === TypedTerm.Const(5))

//    val idId = MonoInterpreter.interpret(λᶜ('x, 'x) apply λˢ('y, 'y))
//    assert(idId === 0)

    val idIdInt = MonoInterpreter.interpret((λᶜ('x, 'x) apply λˢ('y, 'y)) apply 4)
    assert(idIdInt === TypedTerm.Const(4))

  }
}
