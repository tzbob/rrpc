package rpc

import cats.syntax.functor._
import io.circe.generic.JsonCodec
import io.circe.{Decoder, HCursor}
import rpc.Expr.Closed.LamStore
import rpc.Expr.{Closed, Open}

object Declaration {
  case class Binding[Expr](name: String, tpe: Tpe, expr: Expr)
  implicit val bindingDecoder: Decoder[Binding[Expr.Open.Expr]] =
    (c: HCursor) =>
      c.as[(String, Tpe, Expr.Open.Expr)].map {
        case (s, t, e) => Binding(s, t, e)
    }

  case class DataType(name: String,
                      locAbs: List[String],
                      typeAbs: List[String],
                      constructors: List[Constructor])
  implicit val dataType: Decoder[DataType] = c =>
    c.as[(String, List[String], List[String], List[Constructor])]
      .map(DataType.tupled)



  trait TopLevel
  object TopLevel {
    case class Binding[E](b: Declaration.Binding[E]) extends TopLevel
    case class DataType(d: Declaration.DataType)     extends TopLevel
    case class Library(name: String, tpe: Tpe)       extends TopLevel

    def compileBinding(
        b: TopLevel.Binding[Open.Expr],
        lamStore: LamStore): (Binding[Expr.Closed.Expr], LamStore) = {
      val (newExpr, store) = Closed.compileForInterpreter(b.b.expr, lamStore)
      Binding(Declaration.Binding(b.b.name, b.b.tpe, newExpr)) -> store
    }

    implicit val tlBindingD: Decoder[Binding[Expr.Open.Expr]] = (c: HCursor) =>
      c.downField("BindingTopLevel")
        .as[Declaration.Binding[Expr.Open.Expr]]
        .map(Binding[Expr.Open.Expr])
    implicit val tlDataTypeD: Decoder[DataType] = (c: HCursor) =>
      c.downField("DataTypeTopLevel").as[Declaration.DataType].map(DataType)
    implicit val tlLibD: Decoder[Library] = (c: HCursor) =>
      c.downField("LibDeclTopLevel").as[(String, Tpe)].map(Library.tupled)

    implicit val tlD: Decoder[TopLevel] =
      List[Decoder[TopLevel]](tlDataTypeD.widen, tlBindingD.widen, tlLibD.widen)
        .reduceLeft(_ or _)
  }

  case class Constructor(name: String, tpes: List[Tpe])
  implicit val constructorD: Decoder[Constructor] = (c: HCursor) =>
    c.as[(String, List[Tpe])].map(Constructor.tupled)
}
