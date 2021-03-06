package rpc
//
//import org.scalatest.funsuite.AnyFunSuite
//import rpc.Equation.UnifyError
//
//class InferTest extends AnyFunSuite {
//  import Dsl._
//
//  test("Inferring the simple terms should work") {
//
//    assert(Infer.infer(5) === TypedTerm.Const(5))
////    assert(Infer.infer(Term.Var("hello")) === TypedTerm.Var("hello")) should error!
//
//    assert(
//      Infer.infer(Ls("x", "x")(5)) ===
//        TypedTerm.App(
//          TypedLocation.Location(Location.Server),
//          TypedTerm.Lam(Location.Server, "x", Tpe.Int, TypedTerm.Var("x")),
//          TypedTerm.Const(5)))
//
//    assert(
//      Infer.infer(Ls("x", "x")(5)) ===
//        TypedTerm.App(
//          TypedLocation.Location(Location.Server),
//          TypedTerm.Lam(Location.Server, "x", Tpe.Int, TypedTerm.Var("x")),
//          TypedTerm.Const(5)))
//  }
//
//  test("Infer identity through identity") {
//    val term = Infer.infer(λᶜ("x", "x") apply λˢ("y", "y"))
//    assert(
//      term ===
//        TypedTerm.App(
//          TypedLocation.Location(Location.Client),
//          TypedTerm.Lam(Location.Client,
//                        "x",
//                        Tpe.Fun(Tpe.Var(1),
//                                TypedLocation.Location(Location.Server),
//                                Tpe.Var(1)),
//                        TypedTerm.Var("x")),
//          TypedTerm.Lam(Location.Server, "y", Tpe.Var(1), TypedTerm.Var("y"))
//        )
//    )
//  }
//
//  test("Unify locations from fun type comparisons") {
//    val term = Infer.infer(λˢ("f", "f".v apply 1) apply (λᶜ("x", "x")))
////    println(TypedTerm.PrettyTypedShow.show(term))
//
//    val result =
//      TypedTerm.App(
//        TypedLocation.Location(Location.Server),
//        TypedTerm.Lam(
//          Location.Server,
//          "f",
//          Tpe.Fun(Tpe.Var(3),
//                  TypedLocation.Location(Location.Client),
//                  Tpe.Var(3)),
//          TypedTerm.App(
//            TypedLocation.Location(Location.Client),
//            TypedTerm.Var("f"),
//            TypedTerm.Const(1)
//          )
//        ),
//        TypedTerm.Lam(
//          Location.Client,
//          "x",
//          Tpe.Int,
//          TypedTerm.Var("x")
//        )
//      )
//    assert(term === result)
//  }
//
//  test("Paper example p.5") {
//    val x = λˢ("f", λˢ("x", "x")("f".v apply 1))
//
//    val term = Infer.infer(
//      λˢ("f", λˢ("x", "x")("f".v apply 1)) apply λc("y", λs("z", "z") apply "y")
//    )
//
//    assert(
//      TypedTerm.PrettyShow
//        .show(term) === "(λˢf. (λˢx. x) ˢ((f) ᶜ(C1))) ˢ(λᶜy. (λˢz. z) ˢ(y))")
//  }
//
//  test("Regression test #1: term from stackoverflow") {
//    val stacktest = λᶜ("xyz", "xyz") apply λᶜ("x", "x".v apply "x") apply λᶜ(
//      "x",
//      "x") apply 5
//    intercept[UnifyError] {
//      Infer.infer(stacktest)
//    }
//  }
//
//}
