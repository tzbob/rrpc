package rpc

import rpc.Expr.Closed.LamStore
import rpc.Expr.{Closed, Open}
import rpc.error.MissingLibError

case class LibData(expr: Open.Expr, implementation: List[Value] => Value)

trait LibInt {
  import Dsl._
  import Open._
  val sharedFunctions: Map[String, LibData] = Map(
    ("print",
     LibData(
       LocAbs(List("l"),
              Abs(List(("x", '_', 'l')), Native("print", List('x')))), {
         (ls: List[Value]) =>
           println(ls.head)
           Value.Constant(Literal.Unit)
       }
     )),
    ("not",
     LibData(LocAbs(List("l"),
                    Abs(List(("x", BoolTpe, 'l')), Native("not", List('x')))), {
               case List(Value.Constant(Literal.Bool(b))) =>
                 Value.Constant(Literal.Bool(!b))
             })),
    ("ref",
     LibData(
       LocAbs(
         List("l"),
         LocAbs(List("k"),
                TypeAbs(List("a"),
                        Abs(List(("x", 'a', 'k')),
                            App(
                              Abs(List(("u", UnitTpe, 'l')),
                                  Native("ref", List('x'))),
                              Lit(Literal.Unit),
                              Some('l')
                            ))))
       ), { (vs: List[Value]) =>
         vs match {
           case List(value) =>
             val address = RefStorage.init(value)
             Value.Constructed("Ref",
                               List(Value.Constant(Literal.Int(address))))
         }
       }
     )),
    ("!",
     LibData(
       LocAbs(
         List("l"),
         LocAbs(
           List("k"),
           TypeAbs(
             List("a"),
             Abs(List(("x", Tpe.Data("ref", List('l'), List('a')), 'k')),
                 App(
                   Abs(List(("u", UnitTpe, 'l')), Native("!", List('x'))),
                   Lit(Literal.Unit),
                   Some('l')
                 ))
           )
         )
       ), { (vs: List[Value]) =>
         vs match {
           case List(
               Value.Constructed("Ref",
                                 List(Value.Constant(Literal.Int(addr))))) =>
             RefStorage.read(addr)
         }
       }
     )),
    (":=",
     LibData(
       LocAbs(
         List("l"),
         LocAbs(
           List("k"),
           TypeAbs(
             List("a"),
             Abs(
               List(("r", Tpe.Data("ref", List('l'), List('a')), 'k')),
               Abs(List(("p", 'a', 'k')),
                   App(
                     Abs(List(("u", UnitTpe, 'l')),
                         Native(":=", List('r', 'p'))),
                     Lit(Literal.Unit),
                     Some('l')
                   ))
             )
           )
         )
       ), { (vs: List[Value]) =>
         vs match {
           case List(Value.Constructed("Ref",
                                       List(Value.Constant(Literal.Int(addr)))),
                     value) =>
             RefStorage.write(addr, value)
             Value.Constant(Literal.Unit)
         }
       }
     ))
  )

  val nativeFunctions: Map[String, LibData]

  def functions: Map[String, LibData] = sharedFunctions ++ nativeFunctions

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
      case Some(data) =>
        (store: LamStore) =>
          {
            val (expr, nStore) = Closed.compileForInterpreter(data.expr, store)
            expr.asInstanceOf[Closed.LamRef] -> nStore
          }
    }

  def fun(name: String): List[Value] => Value = {
    functions.get(name) match {
      case None       => throw MissingLibError(name)
      case Some(data) => data.implementation
    }
  }
}
