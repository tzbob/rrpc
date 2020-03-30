package rpc

import rpc.Expr.Closed.LamStore
import rpc.Expr.{Closed, Open}
import rpc.error.MissingLibError

trait LibInt {
  val sharedFunctions: Map[String, (List[Tpe], List[Value] => Value)] = Map(
    // FIXME: Add all other todo items
    ("print", List(Tpe.Var("any")) -> { (ls: List[Value]) =>
      println(ls.head)
      Value.Constant(Literal.Unit)
    }))

  val nativeFunctions: Map[String, (List[Tpe], List[Value] => Value)]

  def functions: Map[String, (List[Tpe], List[Value] => Value)] =
    sharedFunctions ++ nativeFunctions

  private def mkAbs(tpes: List[Tpe])(
      bodyF: List[Open.Var] => Open.Expr): Open.Expr = {
    val vars = (1 to tpes.size).map(i => Open.Var(s"##lib_arg_$i")).toList
    Open.LocAbs(List("l"), vars.zip(tpes).reverse.foldLeft(bodyF(vars)) {
      case (body, (v, tpe)) =>
        Open.Abs(List((v.name, tpe, Location.Var("l"))), body)
    })
  }

  def expr(name: String): LamStore => (Closed.LamRef, LamStore) =
    functions.get(name) match {
      case None => throw MissingLibError(name)
      case Some((tpes, _)) =>
        (store: LamStore) => {
          val (expr, nStore) = Closed.compileForInterpreter(mkAbs(tpes) { vars =>
            Open.Native(name, vars)
          }, store)
          expr.asInstanceOf[Closed.LamRef] -> nStore
        }
    }

  def fun(name: String): List[Value] => Value = {
    functions.get(name) match {
      case None         => throw MissingLibError(name)
      case Some((_, f)) => f
    }
  }
}
