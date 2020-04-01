package rpc

import cats.effect.IO
import io.circe.parser._
import org.scalatest.funsuite.AnyFunSuite
import rpc.Declaration.TopLevel
import rpc.Expr.Closed
import rpc.Interpreter.RequestReplyF

class TpeTest extends AnyFunSuite {

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
                                  Tpe.Fun(Tpe.Data("String", Nil, List()),
                                          Location.Var("l"),
                                          Tpe.Data("Unit", Nil, List())))),
      TopLevel.DataType(Declaration.DataType("Unit", Nil, List(), List())),
      TopLevel.DataType(Declaration.DataType("Int", Nil, List(), List())),
      TopLevel.DataType(
        Declaration.DataType("Bool",
                             Nil,
                             List(),
                             List(Declaration.Constructor("True", List()),
                                  Declaration.Constructor("False", List())))
      ),
      TopLevel.DataType(Declaration.DataType("String", Nil, List(), List())),
      TopLevel.Binding(
        Declaration.Binding(
          "main",
          Tpe.Data("Unit", Nil, List()),
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
}
