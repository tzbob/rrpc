package rpc

import rpc.Expr.Closed.LamStore
import rpc.Expr.{Closed, Open}

object Declaration {
  case class Binding[Expr](name: String, tpe: Tpe, expr: Expr)
  case class DataType(name: String,
                      locAbs: List[String],
                      typeAbs: List[String],
                      constructors: List[Constructor])

  trait TopLevel[E]
  object TopLevel {
    case class Binding[E](b: Declaration.Binding[E]) extends TopLevel[E]
    case class DataType[E](d: Declaration.DataType)  extends TopLevel[E]
    case class Library[E](name: String, tpe: Tpe)    extends TopLevel[E]

    def compileBinding(
        b: TopLevel.Binding[Open.Expr],
        lamStore: LamStore): (Binding[Expr.Closed.Expr], LamStore) = {
      val (newExpr, store) = Closed.compileForInterpreter(b.b.expr, lamStore)
      Binding(Declaration.Binding(b.b.name, b.b.tpe, newExpr)) -> store
    }

    def isPage(topLevels: List[TopLevel[Open.Expr]]) =
      topLevels.lastOption match {
        case Some(
            Binding(Declaration.Binding("main", Tpe.Data("Page", _, _), _))) =>
          true
        case _ => false
      }
  }

  case class Constructor(name: String, tpes: List[Tpe])
}
