package rpc

import cats.effect.IO
import io.circe.parser._
import org.scalatest.funsuite.AnyFunSuite
import rpc.Declaration.TopLevel
import rpc.Expr.Closed
import rpc.Interpreter.RequestReplyF

class TpeTest extends AnyFunSuite {

  test("hello world binding top level") {
    val stringSrc =
      """{"BindingTopLevel":
                   ["main",
                    {"ConType": ["Unit", []]},
                    {"App": [{"LocApp": [{"Var": "print"}, [{"Location": "client"}]]},
                             {"Lit": {"StrLit": "\"Hello World\\n\""}},
                             {"Just": {"Location": "client"}}]}]}"""

    val result = decode[Declaration.TopLevel.Binding[Expr.Open.Expr]](stringSrc)

    val target = Right(
      Declaration.TopLevel.Binding(Declaration.Binding(
        "main",
        Tpe.Data("Unit", List()),
        Expr.Open.App(
          Expr.Open.LocApp(Expr.Open.Var("print"),
                           List(Location.Loc("client"))),
          Expr.Open.Lit(Literal.String("\"Hello World\\n\"")),
          Some(Location.Loc("client"))
        )
      )))

    assert(result === target)
  }

  test("Full Hello World") {
    val src =
      """ [{"LibDeclTopLevel": ["print",
                      {"LocAbsType": [["l"],
                                      {"FunType": [{"ConType": ["String", []]}, {"LocVar": "l"},
                                                   {"ConType": ["Unit", []]}]}]}]},
         {"DataTypeTopLevel": ["Unit", [], []]},
         {"DataTypeTopLevel": ["Int", [], []]},
         {"DataTypeTopLevel": ["Bool", [], [["True", []], ["False", []]]]},
         {"DataTypeTopLevel": ["String", [], []]},
         {"BindingTopLevel": ["main", {"ConType": ["Unit", []]},
                              {"App": [{"LocApp": [{"Var": "print"}, [{"Location": "client"}]]},
                                       {"Lit": {"StrLit": "\"Hello World\\n\""}},
                                       {"Just": {"Location": "client"}}]}]}]
        """

    val result = decode[List[TopLevel]](src)

    val target = List(
      TopLevel.Library("print",
                       Tpe.LocAbs(List("l"),
                                  Tpe.Fun(Tpe.Data("String", List()),
                                          Location.Var("l"),
                                          Tpe.Data("Unit", List())))),
      TopLevel.DataType(Declaration.DataType("Unit", List(), List())),
      TopLevel.DataType(Declaration.DataType("Int", List(), List())),
      TopLevel.DataType(
        Declaration.DataType("Bool",
                             List(),
                             List(Declaration.Constructor("True", List()),
                                  Declaration.Constructor("False", List())))
      ),
      TopLevel.DataType(Declaration.DataType("String", List(), List())),
      TopLevel.Binding(
        Declaration.Binding(
          "main",
          Tpe.Data("Unit", List()),
          Expr.Open.App(
            Expr.Open.LocApp(Expr.Open.Var("print"),
                             List(Location.Loc("client"))),
            Expr.Open.Lit(Literal.String("\"Hello World\\n\"")),
            Some(Location.Loc("client"))
          )
        )
      )
    )

    assert(result === Right(target))
  }

  implicit val config =
    PolyRpcCaller.Config("jvm/src/test/resources/examples", "rl")

  test("Test Full Hello World Run") {
    val jsonLoad = PolyRpcCaller.load("helloworld")
    val decls    = decode[List[TopLevel]](jsonLoad)

    val test = decls.toOption.get.foldLeft(IO.pure(Map.empty[Closed.Var, Value])) {
      (accIO, dec) =>
        accIO.flatMap { acc =>
          Interpreter.processDeclaration(dec, acc) { store =>
            val (c, v) = TestRunner.fullRunIOFunctions(store)
            RequestReplyF(c, v)
          }
        }
    }.unsafeRunSync()

    pprint.log(test)

    assert(test === 1)
  }

}
