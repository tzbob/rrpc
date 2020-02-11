package rpc

import cats.Show

sealed trait Tpe

object Tpe {
  case object Int                                    extends Tpe
  case class Var(id: Int)                            extends Tpe
  case class Fun(a: Tpe, loc: TypedLocation, b: Tpe) extends Tpe

  implicit object PrettyTpe extends Show[Tpe] {
    override def show(t: Tpe): String = t match {
      case Int            => "Int"
      case Var(id)        => s"Var$id"
      case Fun(a, loc, b) => s"${show(a)} ->${loc.superscript} ${show(b)}"
    }
  }
}
