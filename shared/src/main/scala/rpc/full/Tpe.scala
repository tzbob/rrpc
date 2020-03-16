package rpc.full

import cats.syntax.functor._
import io.circe.{Decoder, DecodingFailure, HCursor}

//sealed trait Location
//object Location {
//  case class Var(name: String) extends Location
//  case class Loc(name: String) extends Location
//
//  implicit val locVarD: Decoder[Var] = (c: HCursor) =>
//    c.downField("LocVar").as[String].map(Var)
//  implicit val locLocD: Decoder[Loc] = (c: HCursor) =>
//    c.downField("Location").as[String].map(Loc.apply)
//  implicit val locD: Decoder[Location] =
//    List[Decoder[Location]](locVarD.widen, locLocD.widen).reduceLeft(_ or _)
//}
//
//sealed trait Tpe
//object Tpe {
//  case class Var(str: String)                                  extends Tpe
//  case class Tup(tpes: List[Tpe])                              extends Tpe
//  case class Fun(a: Tpe, loc: Location, b: Tpe)                extends Tpe
//  case class TypeAbs(abstractions: List[String], tpeBody: Tpe) extends Tpe
//  case class LocAbs(abstractions: List[String], tpeBody: Tpe)  extends Tpe
//  case class Data(name: String, fields: List[Tpe])             extends Tpe
//
//  implicit val tpeVarD: Decoder[Var] = (c: HCursor) =>
//    c.downField("TypeVarType").as[String].map(Var)
//  implicit val tpeTupleD: Decoder[Tup] = (c: HCursor) =>
//    c.downField("TupleType").as[List[Tpe]].map(Tup.apply)
//  implicit val tpeFunD: Decoder[Fun] = (c: HCursor) =>
//    c.downField("FunType").as[(Tpe, Location, Tpe)].map(Fun.tupled)
//  implicit val tpeAbsD: Decoder[TypeAbs] = (c: HCursor) =>
//    c.downField("TypeAbsType").as[(List[String], Tpe)].map(TypeAbs.tupled)
//  implicit val tpeLocAbsD: Decoder[LocAbs] = (c: HCursor) =>
//    c.downField("LocAbsType").as[(List[String], Tpe)].map(LocAbs.tupled)
//  implicit val tpeDataD: Decoder[Data] = (c: HCursor) =>
//    c.downField("ConType").as[(String, List[Tpe])].map(Data.tupled)
//
//  implicit val tpeD: Decoder[Tpe] =
//    List[Decoder[Tpe]](tpeVarD.widen,
//                       tpeTupleD.widen,
//                       tpeFunD.widen,
//                       tpeAbsD.widen,
//                       tpeLocAbsD.widen,
//                       tpeDataD.widen).reduceLeft(_ or _)
//}

//sealed trait Expr
//object Expr {
//  case class Var(str: String)                                     extends Expr
//  case class TypeAbs(strs: List[String], expr: Expr)              extends Expr
//  case class LocAbs(strs: List[String], expr: Expr)               extends Expr
//  case class Abs(abss: List[(String, Tpe, Location)], expr: Expr) extends Expr
//  case class Let(binding: Declaration.Binding, expr: Expr)        extends Expr
//  case class Case(expr: Expr, alts: List[Alternative])            extends Expr
//  case class App(a: Expr, b: Expr, location: Option[Location])    extends Expr
//  case class TypeApp(expr: Expr, tpes: List[Tpe])                 extends Expr
//  case class LocApp(expr: Expr, locs: List[Location])             extends Expr
//  case class Tup(list: List[Expr])                                extends Expr
//  case class Prim(op: Operator, args: List[Expr])                 extends Expr
//  case class Lit(literal: Literal)                                extends Expr
//  case class Data(name: String, tpes: List[Tpe], expr: Expr)      extends Expr
//
//  implicit val exprTupleD: Decoder[Var] = (c: HCursor) =>
//    c.downField("Var").as[String].map(Var.apply)
//  implicit val exprTypeAbsD: Decoder[TypeAbs] = (c: HCursor) =>
//    c.downField("TypeAbs").as[(List[String], Expr)].map(TypeAbs.tupled)
//  implicit val exprLocAbsD: Decoder[LocAbs] = (c: HCursor) =>
//    c.downField("LocAbs").as[(List[String], Expr)].map(LocAbs.tupled)
//  implicit val exprAbsD: Decoder[Abs] = (c: HCursor) =>
//    c.downField("Abs").as[(List[(String, Tpe, Location)], Expr)].map(Abs.tupled)
//  implicit val exprLetD: Decoder[Let] = (c: HCursor) =>
//    c.downField("Let").as[(Declaration.Binding, Expr)].map(Let.tupled)
//  implicit val exprCaseD: Decoder[Case] = (c: HCursor) =>
//    c.downField("Case").as[(Expr, List[Alternative])].map(Case.tupled)
//  implicit val exprAppD: Decoder[App] = (c: HCursor) => {
//    implicit def optionSome: Decoder[Option[Location]] = {
//      val some: Decoder[Some[Location]] = (c: HCursor) =>
//        c.downField(("Just")).as[Location].map(Some.apply)
//      val none: Decoder[None.type] = (c: HCursor) => {
//        if (c.downField("Nothing").succeeded)
//          Left(DecodingFailure(s"Cannot decode App", c.history))
//        else Right(None)
//      }
//      some or none.widen
//    }
//    c.downField("App").as[(Expr, Expr, Option[Location])].map(App.tupled)
//  }
//  implicit val exprTypeAppD: Decoder[TypeApp] = (c: HCursor) =>
//    c.downField("TypeApp").as[(Expr, List[Tpe])].map(TypeApp.tupled)
//  implicit val exprLocAppD: Decoder[LocApp] = (c: HCursor) =>
//    c.downField("LocApp").as[(Expr, List[Location])].map(LocApp.tupled)
//  implicit val exprTupD: Decoder[Tup] = (c: HCursor) =>
//    c.downField("Tup").as[List[Expr]].map(Tup)
//  implicit val exprPrimD: Decoder[Prim] = (c: HCursor) =>
//    c.downField("Prim").as[(Operator, List[Expr])].map(Prim.tupled)
//  implicit val exprLitD: Decoder[Lit] = (c: HCursor) =>
//    c.downField("Lit").as[Literal].map(Lit)
//  implicit val exprDataD: Decoder[Data] = (c: HCursor) =>
//    c.downField("Constr").as[(String, List[Tpe], Expr)].map(Data.tupled)
//
//  implicit val exprD: Decoder[Expr] =
//    List[Decoder[Expr]](
//      exprTupleD.widen,
//      exprTypeAbsD.widen,
//      exprLocAbsD.widen,
//      exprAbsD.widen,
//      exprLetD.widen,
//      exprCaseD.widen,
//      exprTypeAppD.widen,
//      exprLocAppD.widen,
//      exprTupD.widen,
//      exprPrimD.widen,
//      exprLitD.widen,
//      exprDataD.widen,
//      exprAppD.widen,
//    ).reduceLeft(_ or _)
//}
//
//case class Alternative(name: String, params: List[String], expr: Expr)
//object Alternative {
//  implicit val alternativeD: Decoder[Alternative] =
//    (c: HCursor) =>
//      c.as[(String, List[String], Expr)].map {
//        case (str, strs, expr) =>
//          Alternative(str, strs, expr)
//    }
//}
//
//sealed trait Literal
//object Literal {
//  case class Int(i: scala.Int)           extends Literal
//  case class String(s: java.lang.String) extends Literal
//  case class Bool(b: Boolean)            extends Literal
//  case object Unit                       extends Literal
//
//  implicit val litIntD: Decoder[Int] = (c: HCursor) =>
//    c.downField("IntLit").as[scala.Int].map(Int.apply)
//  implicit val litStringD: Decoder[String] = (c: HCursor) =>
//    c.downField("StrLit").as[java.lang.String].map(String.apply)
//  implicit val litBoolD: Decoder[Bool] = (c: HCursor) =>
//    c.downField("BoolLit").as[Boolean].map(Bool.apply)
//  implicit val litUnitD: Decoder[Literal.Unit.type] = (c: HCursor) => {
//    if (c.downField("UnitLit").succeeded)
//      Left(DecodingFailure(s"Cannot decode $c into UnitLit", c.history))
//    else Right(Literal.Unit)
//  }
//
//  implicit val litD: Decoder[Literal] =
//    List[Decoder[Literal]](litBoolD.widen,
//                           litIntD.widen,
//                           litStringD.widen,
//                           litUnitD.widen).reduceLeft(_ or _)
//}
//
//sealed trait Operator
//object Operator {
//  private def opD(op: Operator): Decoder[Operator] =
//    (c: HCursor) => {
//      val opName = op.getClass.getSimpleName.dropRight(1)
//      if (c.downField(opName).succeeded)
//        Left(DecodingFailure(s"Cannot decode $op", c.history))
//      else Right(op)
//    }
//
//  case object Not extends Operator //{l}. Bool -l-> Bool
//  case object Or  extends Operator //{l}. (Bool, Bool) -l-> Bool
//  case object And extends Operator //{l}. (Bool, Bool) -l-> Bool
//  case object Eq  extends Operator //{l}. (Bool, Bool) -l-> Bool
//  case object Neq extends Operator //{l}. (Bool, Bool) -l-> Bool
//  case object Lt  extends Operator //{l}. (Int, Int) -l-> Bool
//  case object Le  extends Operator //{l}. (Int, Int) -l-> Bool
//  case object Gt  extends Operator //{l}. (Int, Int) -l-> Bool
//  case object Ge  extends Operator //{l}. (Int, Int) -l-> Bool
//  case object Add extends Operator //{l}. (Int, Int) -l-> Int
//  case object Sub extends Operator //{l}. (Int, Int) -l-> Int
//  case object Mul extends Operator //{l}. (Int, Int) -l-> Int
//  case object Div extends Operator //{l}. (Int, Int) -l-> Int
//  case object Neg extends Operator //{l}. Int -l-> Int
//
//  implicit val opDImpl: Decoder[Operator] =
//    List(Not, Or, And, Eq, Neq, Lt, Le, Gt, Ge, Add, Sub, Mul, Div, Neg)
//      .map(opD)
//      .reduceLeft(_ or _)
//}
//
//object Declaration {
//  case class Binding(name: String, tpe: Tpe, expr: Expr)
//  implicit val bindingDecoder: Decoder[Binding] = (c: HCursor) =>
//    c.as[(String, Tpe, Expr)].map(Binding.tupled)
//
//  case class DataType(name: String,
//                      typeAbs: List[String],
//                      constructors: List[Constructor])
//  implicit val dataType: Decoder[DataType] = c =>
//    c.as[(String, List[String], List[Constructor])].map(DataType.tupled)
//
//  trait TopLevel
//  object TopLevel {
//    case class Binding(b: Declaration.Binding)   extends TopLevel
//    case class DataType(d: Declaration.DataType) extends TopLevel
//    case class Library(name: String, tpe: Tpe)   extends TopLevel
//
//    implicit val tlBindingD: Decoder[Binding] = (c: HCursor) =>
//      c.downField("BindingTopLevel").as[Declaration.Binding].map(Binding)
//    implicit val tlDataTypeD: Decoder[DataType] = (c: HCursor) =>
//      c.downField("DataTypeTopLevel").as[Declaration.DataType].map(DataType)
//    implicit val tlLibD: Decoder[Library] = (c: HCursor) =>
//      c.downField("LibDeclTopLevel").as[(String, Tpe)].map(Library.tupled)
//
//    implicit val tlD: Decoder[TopLevel] =
//      List[Decoder[TopLevel]](tlBindingD.widen, tlDataTypeD.widen, tlLibD.widen)
//        .reduceLeft(_ or _)
//  }
//
//  case class Constructor(name: String, tpes: List[Tpe])
//  implicit val constructorD: Decoder[Constructor] = (c: HCursor) =>
//    c.as[(String, List[Tpe])].map(Constructor.tupled)
//}
