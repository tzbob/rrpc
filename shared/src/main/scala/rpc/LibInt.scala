package rpc

import rpc.Expr.Closed.LamStore
import rpc.Expr.{Closed, Open}
import rpc.error.MissingLibError

case class LibData[E](expr: E, implementation: List[Value] => Value)

trait LibInt {
  import Dsl._
  import Open._
  val sharedFunctions: Map[String, LibData[Open.Expr]] = Map(
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
                              None,
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
                   None,
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
                     None,
                     Lit(Literal.Unit),
                     Some('l')
                   ))
             )
           )
         )
       ), { (vs: List[Value]) =>
         pprint.log(vs)
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

  val nativeFunctions: Map[String, LibData[Open.Expr]]

  def functions: Map[String, LibData[Open.Expr]] =
    sharedFunctions ++ nativeFunctions

  private lazy val (store, lamRefs): (LamStore, Map[String, Closed.LamRef]) =
    functions.foldLeft(LamStore.empty -> Map.empty[String, Closed.LamRef]) {
      case ((store, map), (key, data)) =>
        val (expr, nStore) = Closed.compileForInterpreter(data.expr, store)
        // this is fine since all functions _should_ compile to lamrefs
        val lamRef = expr.asInstanceOf[Closed.LamRef]
        nStore -> (map + (key -> lamRef))
    }

  lazy val libraryStore = store

  def openExpr(name: String) = functions.get(name) match {
    case None       => throw MissingLibError(name)
    case Some(data) => data.expr
  }

  def expr(name: String): Closed.LamRef =
    lamRefs.get(name) match {
      case None     => throw MissingLibError(name)
      case Some(lr) => lr
    }

  def fun(name: String): List[Value] => Value = {
    functions.get(name) match {
      case None       => throw MissingLibError(name)
      case Some(data) => data.implementation
    }
  }
}
