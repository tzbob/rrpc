package rpc

import cats.effect.IO
import rpc.Expr.{Closed, Open}
import rpc.Expr.Closed.{ClosedLam, LamRef}
import rpc.Expr.Open._
import rpc.Interpreter._
import rpc.Value.Closure
import io.circe.syntax._
import io.circe.parser.decode
import rpc.Declaration.TopLevel

class HaskellJsonDecodersTest extends org.scalatest.FunSuite {
  import Dsl._

  val tInt = Tpe.Data("Int", Nil, Nil)

//  test("Decoder for recursive environment should work") {
//    val id = TopLevel.Binding(
//      Declaration
//        .Binding[Open.Expr]("recurse", Tpe.Var("a"), λˢ('t', tInt)(Open.Var("recurse"))))
//
//    val (List(binding), store) = Interpreter.compileDeclarations(List(id))
//    val b                      = binding.asInstanceOf[TopLevel.Binding[Closed.LamRef]]
//
//    val env = Env.empty
//    val nEnv = Closure.addRecursiveClosure(b.b.name, b.b.expr, env, store)
//    val mnEnv = Env.minimize(nEnv, Nil, Nil, List(Closed.Var("recurse")))
//
//    pprint.log(mnEnv.asJson)
//    assert(false)
//  }
}
