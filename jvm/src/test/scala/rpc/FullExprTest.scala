package rpc

import io.circe.parser._
import org.scalatest.funsuite.AnyFunSuite
import rpc.Declaration.TopLevel
import rpc.Expr.Open
import rpc.error.CaseError

class FullExprTest extends AnyFunSuite {
  implicit val config =
    PolyRpcCaller.Config("jvm/src/main/resources/examples", "rl")

  private def runProgram(file: String, reset: Boolean = false) = {
    val jsonLoad = PolyRpcCaller.load(file, reset)

    import HaskellJsonDecoders._
    val decls    = decode[List[TopLevel[Open.Expr]]](jsonLoad)

    decls match {
      case Right(value) =>
        // Separate store for servers as it would be in an actual run.
        val (_, store) = Interpreter.compileDeclarations(value)
        Interpreter
          .runDeclarations(value, TestRunner.fullRunIOFunctions(store))
          .unsafeRunSync()

      case Left(error) => throw new RuntimeException(error)
    }
  }

  test("Test Full Hello World Run") {
    val result = runProgram("helloworld")
    assert(result.values("main") === Value.Constant(Literal.Unit))
  }

  test("Test Let") {
    val result = runProgram("hellolet")
    assert(result.values("main") === Value.Constant(Literal.Int(20)))
  }

  test("Test Tuple") {
    val result = runProgram("tup")
    assert(result.values("main") === Value.Constant(Literal.Int(5)))
  }

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
    intercept[CaseError] { runProgram("missingcase") }
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

  test("Cross-tier calls") {
    import Value._
    val result = runProgram("cross")
    assert(result.values("main") === Constant(Literal.Int(200)))
  }

  test("Test Recursive Values + Case + Constructor") {
    import Value._

    val result = runProgram("map")
    assert(
      result.values("main") ===
        Constructed(
          "Cons",
          List(
            Constant(Literal.Int(1)),
            Constructed("Cons",
                        List(Constant(Literal.Int(2)),
                             Constructed("Cons",
                                         List(Constant(Literal.Int(3)),
                                              Constructed("Nil", List())))))
          )
        ))
  }

  test("Test Arrow") {
    import Dsl._
    import Value._
    val result = runProgram("arrow")
    assert(result.values("main") === Constant(8))
  }

  test("Test Stream") {
    import Dsl._
    import Value._
    val result = runProgram("stream")
    assert(result.values("test1") === Constant(3))
  }

  test("Test Ref") {
    import Dsl._
    import Value._
    val result = runProgram("ref")
    assert(result.values("main") === Constant("\"four five six\""))
  }
}
