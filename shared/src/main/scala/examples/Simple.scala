package examples

import rpc._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object Simple extends RpcApp with RpcAppInt {

  val hostname = "localhost"
  val port = 8080
  def rpc(args: List[String]): Expr.Open.Expr = {

    val exampleHelloWorld =
      """
        [{"LibDeclTopLevel": ["print",
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

    val parsedEx = decode[List[Declaration.TopLevel]](exampleHelloWorld)

    // go through all top level decls to build environment, then execute main
    ???

  }

}
