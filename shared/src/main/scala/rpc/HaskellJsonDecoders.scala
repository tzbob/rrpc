package rpc

import cats.syntax.functor._
import io.circe.{Decoder, DecodingFailure, HCursor}
import Declaration._
import rpc.Expr.Open
import rpc.Expr.Open._
import Operator._

object HaskellJsonDecoders {
  private implicit val litIntD: Decoder[Literal.Int] = (c: HCursor) =>
    c.downField("IntLit").as[scala.Int].map(Literal.Int.apply)
  private implicit val litStringD: Decoder[Literal.String] = (c: HCursor) =>
    c.downField("StrLit").as[java.lang.String].map(Literal.String.apply)
  private implicit val litBoolD: Decoder[Literal.Bool] = (c: HCursor) =>
    c.downField("BoolLit").as[Boolean].map(Literal.Bool.apply)

  // FIXME: this always succeeds into making a unit
  private implicit val litUnitD: Decoder[Literal.Unit.type] = (c: HCursor) => {
    if (c.downField("UnitLit").succeeded)
      Left(DecodingFailure(s"Cannot decode $c into UnitLit", c.history))
    else Right(Literal.Unit)
  }
  implicit val litD: Decoder[Literal] =
    List[Decoder[Literal]](litIntD.widen,
                           litStringD.widen,
                           litBoolD.widen,
                           litUnitD.widen).reduceLeft(_ or _)

  private implicit val tpeVarD: Decoder[Tpe.Var] = (c: HCursor) =>
    c.downField("TypeVarType").as[String].map(Tpe.Var)
  private implicit val tpeTupleD: Decoder[Tpe.Tup] = (c: HCursor) =>
    c.downField("TupleType").as[List[Tpe]].map(Tpe.Tup.apply)
  private implicit val tpeFunD: Decoder[Tpe.Fun] = (c: HCursor) =>
    c.downField("FunType").as[(Tpe, Location, Tpe)].map(Tpe.Fun.tupled)
  private implicit val tpeAbsD: Decoder[Tpe.TypeAbs] = (c: HCursor) =>
    c.downField("TypeAbsType").as[(List[String], Tpe)].map(Tpe.TypeAbs.tupled)
  private implicit val tpeLocAbsD: Decoder[Tpe.LocAbs] = (c: HCursor) =>
    c.downField("LocAbsType").as[(List[String], Tpe)].map(Tpe.LocAbs.tupled)
  private implicit val tpeDataD: Decoder[Tpe.Data] = (c: HCursor) =>
    c.downField("ConType")
      .as[(String, List[Location], List[Tpe])]
      .map(Tpe.Data.tupled)

  implicit val tpeD: Decoder[Tpe] =
    List[Decoder[Tpe]](tpeVarD.widen,
                       tpeTupleD.widen,
                       tpeFunD.widen,
                       tpeAbsD.widen,
                       tpeDataD.widen,
                       tpeLocAbsD.widen).reduceLeft(_ or _)

  private implicit val altD: Decoder[Alternative.Alt[Expr.Open.Expr]] =
    (c: HCursor) =>
      c.downField("Alternative")
        .as[(String, List[String], Expr.Open.Expr)]
        .map {
          case (str, strs, expr) => Alternative.Alt(str, strs, expr)
      }

  private implicit val tupleAltD: Decoder[Alternative.TupAlt[Expr.Open.Expr]] =
    (c: HCursor) =>
      c.downField("TupleAlternative").as[(List[String], Expr.Open.Expr)].map {
        case (strs, expr) => Alternative.TupAlt(strs, expr)
    }

  implicit val alternativeD: Decoder[Alternative[Expr.Open.Expr]] =
    List[Decoder[Alternative[Expr.Open.Expr]]](
      altD.widen,
      tupleAltD.widen
    ).reduceLeft(_ or _)

  implicit val bindingDecoder: Decoder[Declaration.Binding[Expr.Open.Expr]] =
    (c: HCursor) =>
      c.as[(String, Tpe, Expr.Open.Expr)].map {
        case (s, t, e) => Declaration.Binding(s, t, e)
    }

  implicit val dataType: Decoder[DataType] = c =>
    c.as[(String, List[String], List[String], List[Declaration.Constructor])]
      .map(DataType.tupled)

  implicit val tlBindingD: Decoder[TopLevel.Binding[Open.Expr]] =
    (c: HCursor) =>
      c.downField("BindingTopLevel")
        .as[Declaration.Binding[Expr.Open.Expr]]
        .map(TopLevel.Binding[Expr.Open.Expr])
  implicit val tlDataTypeD: Decoder[TopLevel.DataType[Open.Expr]] =
    (c: HCursor) =>
      c.downField("DataTypeTopLevel")
        .as[Declaration.DataType]
        .map(TopLevel.DataType[Open.Expr])
  implicit val tlLibD: Decoder[TopLevel.Library[Open.Expr]] = (c: HCursor) =>
    c.downField("LibDeclTopLevel")
      .as[(String, Tpe)]
      .map(x => (TopLevel.Library.apply[Open.Expr] _).tupled(x))

  implicit val tlD: Decoder[TopLevel[Open.Expr]] =
    List[Decoder[TopLevel[Open.Expr]]](tlDataTypeD.widen,
                                       tlBindingD.widen,
                                       tlLibD.widen)
      .reduceLeft(_ or _)

  implicit val constructorD: Decoder[Declaration.Constructor] = (c: HCursor) =>
    c.as[(String, List[Tpe])].map(Declaration.Constructor.tupled)

  implicit val someLocation: Decoder[Option[Location]] = {
    val some: Decoder[Some[Location]] = (c: HCursor) =>
      c.downField(("Just")).as[Location].map(Some.apply)
    val none: Decoder[None.type] = (c: HCursor) => {
      if (c.downField("Nothing").succeeded)
        Left(DecodingFailure(s"Cannot decode App", c.history))
      else Right(None)
    }
    some or none.widen
  }
  implicit val someTpe: Decoder[Option[Tpe]] = {
    val some: Decoder[Some[Tpe]] = (c: HCursor) =>
      c.downField(("Just")).as[Tpe].map(Some.apply)
    val none: Decoder[None.type] = (c: HCursor) => {
      if (c.downField("Nothing").succeeded)
        Left(DecodingFailure(s"Cannot decode App", c.history))
      else Right(None)
    }
    some or none.widen
  }

  private implicit val exprVarD: Decoder[Var] = (c: HCursor) =>
    c.downField("Var").as[String].map(Var.apply)
  private implicit val exprTypeAbsD: Decoder[TypeAbs] = (c: HCursor) =>
    c.downField("TypeAbs").as[(List[String], Expr)].map(TypeAbs.tupled)
  private implicit val exprLocAbsD: Decoder[LocAbs] = (c: HCursor) =>
    c.downField("LocAbs").as[(List[String], Expr)].map(LocAbs.tupled)
  private implicit val exprAbsD: Decoder[Abs] = (c: HCursor) =>
    c.downField("Abs")
      .as[(List[(String, Tpe, Location)], Expr)]
      .map(Abs.tupled)
  private implicit val exprLetD: Decoder[Let] = (c: HCursor) =>
    c.downField("Let")
      .as[(List[Declaration.Binding[Open.Expr]], Expr)]
      .map(Let.tupled)
  private implicit val exprCaseD: Decoder[Case] = (c: HCursor) =>
    c.downField("Case")
      .as[(Expr, Option[Tpe], List[Alternative[Expr.Open.Expr]])]
      .map(Case.tupled)
  private implicit val exprTypeAppD: Decoder[TypeApp] = (c: HCursor) =>
    c.downField("TypeApp")
      .as[(Expr, Option[Tpe], List[Tpe])]
      .map(TypeApp.tupled)
  private implicit val exprLocAppD: Decoder[LocApp] = (c: HCursor) =>
    c.downField("LocApp")
      .as[(Expr, Option[Tpe], List[Location])]
      .map(LocApp.tupled)
  private implicit val exprTupD: Decoder[Tup] = (c: HCursor) =>
    c.downField("Tuple").as[List[Expr]].map(Tup)
  private implicit val exprPrimD: Decoder[Prim] = (c: HCursor) =>
    c.downField("Prim").as[(Operator, List[Expr])].map(Prim.tupled)
  private implicit val exprLitD: Decoder[Lit] = (c: HCursor) =>
    c.downField("Lit").as[Literal].map(Lit)
  private implicit val exprDataD: Decoder[Open.Constructor] = (c: HCursor) =>
    c.downField("Constr")
      .as[(String, List[Location], List[Tpe], List[Expr])]
      .map(Open.Constructor.tupled)
  private implicit val exprAppD: Decoder[App] = (c: HCursor) => {
    c.downField("App")
      .as[(Expr, Option[Tpe], Expr, Option[Location])]
      .map(App.tupled)
  }
  implicit val exprD: Decoder[Expr] =
    List[Decoder[Expr]](
      exprVarD.widen,
      exprTypeAbsD.widen,
      exprLocAbsD.widen,
      exprAbsD.widen,
      exprCaseD.widen,
      exprTypeAppD.widen,
      exprLocAppD.widen,
      exprTupD.widen,
      exprLitD.widen,
      exprDataD.widen,
      exprAppD.widen,
      exprLetD.widen,
      exprPrimD.widen,
    ).reduceLeft(_ or _)

  implicit val locVarD: Decoder[Location.Var] = (c: HCursor) =>
    c.downField("LocVar").as[String].map(Location.Var)
  implicit val locLocD: Decoder[Location.Loc] = (c: HCursor) =>
    c.downField("Location").as[String].map(Location.Loc.apply)
  implicit val locD: Decoder[Location] =
    List[Decoder[Location]](locVarD.widen, locLocD.widen).reduceLeft(_ or _)

  private def opD(op: Operator): Decoder[Operator] =
    (c: HCursor) => {
      val opName = op.getClass.getSimpleName.dropRight(1)
      c.as[String].flatMap { str =>
        if (str != s"${opName}PrimOp")
          Left(DecodingFailure(s"Cannot decode $op", c.history))
        else Right(op)
      }
    }

  implicit val opDImpl: Decoder[Operator] =
    List(Not, Or, And, Eq, Neq, Lt, Le, Gt, Ge, Add, Sub, Mul, Div, Neg)
      .map(opD)
      .reduceLeft(_ or _)
}
