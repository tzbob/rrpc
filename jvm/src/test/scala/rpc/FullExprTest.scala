package rpc

import cats.effect.IO
import io.circe
import io.circe.parser._
import org.scalatest.funsuite.AnyFunSuite
import rpc.Declaration.TopLevel
import rpc.Interpreter.RequestReplyF
import rpc.error.CaseError

class FullExprTest extends AnyFunSuite {
  implicit val config =
    PolyRpcCaller.Config("jvm/src/test/resources/examples", "rl")

  private def runProgram(file: String, reset: Boolean = false) = {
    val jsonLoad = PolyRpcCaller.load(file, reset)
    val decls    = decode[List[TopLevel]](jsonLoad)

    val result = Interpreter
      .runDeclarations(decls.toOption.get) { store =>
        val (c, v) = TestRunner.fullRunIOFunctions(store)
        RequestReplyF(c, v)
      }
      .unsafeRunSync()
    assert(result.values.contains("main"))
    result
  }

  test("Test Full Hello World Run") {
    val result = runProgram("helloworld")
    assert(result.values("main") === Value.Constant(Literal.Unit))
  }

  test("Test Let") {
    val result = runProgram("hellolet")
    assert(result.values("main") === Value.Constant(Literal.Int(20)))
  }

//   FIXME: Tuple errors in polyrpc-exe?
//  test("Test Tuple") {
//    val result = runProgram("tup")
//    assert(
//      result.values("main") === Value.Tupled(
//        List(Value.Constant(Literal.Int(5)),
//             Value.Constant(Literal.String("hello")))))
//  }

  test("Test Constructor") {
    import Value._
    val result = runProgram("constructor")
    assert(
      result.values("main") === Constructed("Cons",
                                            List(Constant(Literal.Int(1)),
                                                 Constructed("Nil", Nil))))
  }

  test("Test getorelse (constructor, case)") {
    import Value._
    val result = runProgram("getorelse")
    assert(result.values("main") === Constant(Literal.Int(20)))
  }

  test("Test head (multiple constructor, case)") {
    import Value._
    val result = runProgram("head")
    assert(
      result.values("main") === Constructed("Something",
                                            List(Constant(Literal.Int(22)))))
  }

  test("Test missing case") {
    intercept[CaseError] {
      runProgram("missingcase")
    }
  }

  test("Test missing operators") {
    import Value._
    assert(
      runProgram("operators").values("main") === Constant(Literal.Bool(true)))
  }

  test("Test Recursive Values") {
    import Value._

    val result = runProgram("count")
    assert(result.values("main") === Constant(Literal.Int(3)))
  }
  test("Test Recursive Values + Case + Constructor") {
    import Value._

    val result = runProgram("map")
    assert(
      result.values("main") === Constructed("Cons",
                                            List(Constant(Literal.Int(1)),
                                                 Constructed("Nil", Nil))))
  }
}
