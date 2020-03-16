package rpc

import cats.syntax.functor._
import io.circe.generic.JsonCodec
import io.circe.{Decoder, DecodingFailure, HCursor}
import rpc.Expr.Open

trait ExprData {
  sealed trait Expr

  case class Var(name: String) extends Expr
  case class App(fun: Expr, param: Expr, location: Option[Location])
      extends Expr
  case class TypeAbs(strs: List[String], expr: Expr)                extends Expr
  case class LocAbs(strs: List[String], expr: Expr)                 extends Expr
  case class TypeApp(expr: Expr, tpes: List[Tpe])                   extends Expr
  case class LocApp(expr: Expr, locs: List[Location])               extends Expr
  case class Tup(list: List[Expr])                                  extends Expr
  case class Prim(op: Operator, args: List[Expr])                   extends Expr
  case class Lit(literal: Literal)                                  extends Expr
  case class Constructor(name: String, tpes: List[Tpe], expr: Expr) extends Expr
}

object Expr {

  object Open extends ExprData {
    case class Case(expr: Expr, alts: List[Alternative[Open.Expr]]) extends Expr
    case class Let(binding: Declaration.Binding[Open.Expr], expr: Expr)
        extends Expr
    case class Abs(abss: List[(String, Tpe, Location)], expr: Expr) extends Expr

    private implicit val prioritizedLocationDecoder: Decoder[Location] = Location.locD

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
        .as[(Declaration.Binding[Open.Expr], Expr)]
        .map(Let.tupled)
    private implicit val exprCaseD: Decoder[Case] = (c: HCursor) =>
      c.downField("Case")
        .as[(Expr, List[Alternative[Expr.Open.Expr]])]
        .map(Case.tupled)
    private implicit val exprTypeAppD: Decoder[TypeApp] = (c: HCursor) =>
      c.downField("TypeApp").as[(Expr, List[Tpe])].map(TypeApp.tupled)
    private implicit val exprLocAppD: Decoder[LocApp] = (c: HCursor) =>
      c.downField("LocApp").as[(Expr, List[Location])].map(LocApp.tupled)
    private implicit val exprTupD: Decoder[Tup] = (c: HCursor) =>
      c.downField("Tup").as[List[Expr]].map(Tup)
    private implicit val exprPrimD: Decoder[Prim] = (c: HCursor) =>
      c.downField("Prim").as[(Operator, List[Expr])].map(Prim.tupled)
    private implicit val exprLitD: Decoder[Lit] = (c: HCursor) =>
      c.downField("Lit").as[Literal].map(Lit)
    private implicit val exprDataD: Decoder[Constructor] = (c: HCursor) =>
      c.downField("Constr")
        .as[(String, List[Tpe], Expr)]
        .map(Constructor.tupled)
    private implicit val exprAppD: Decoder[App] = (c: HCursor) => {
      implicit val optionSome: Decoder[Option[Location]] = {
        val some: Decoder[Some[Location]] = (c: HCursor) =>
          c.downField(("Just")).as[Location].map(Some.apply)
        val none: Decoder[None.type] = (c: HCursor) => {
          if (c.downField("Nothing").succeeded)
            Left(DecodingFailure(s"Cannot decode App", c.history))
          else Right(None)
        }
        some or none.widen
      }
      c.downField("App").as[(Expr, Expr, Option[Location])].map(App.tupled)
    }
    implicit val exprD: Decoder[Expr] =
      List[Decoder[Expr]](
        exprVarD.widen,
        exprTypeAbsD.widen,
        exprLocAbsD.widen,
        exprAbsD.widen,
        exprLetD.widen,
        exprCaseD.widen,
        exprTypeAppD.widen,
        exprLocAppD.widen,
        exprTupD.widen,
        exprPrimD.widen,
        exprLitD.widen,
        exprDataD.widen,
        exprAppD.widen,
      ).reduceLeft(_ or _)

    def freeVariables(interTerm: Open.Expr): List[Open.Var] = {
      def helper(expr: Open.Expr, bounded: Set[Open.Var]): List[Open.Var] = {
        expr match {
          case v @ Open.Var(_) => if (bounded contains v) Nil else List(v)
          case Open.App(a, b, _) =>
            helper(a, bounded) ::: helper(b, bounded)
          case Open.TypeAbs(_, expr) => helper(expr, bounded)
          case Open.LocAbs(_, expr)  => helper(expr, bounded)
          case Open.Let(Declaration.Binding(name, _, bExpr), expr) =>
            helper(bExpr, bounded) ::: helper(expr, bounded + Var(name))
          case Open.Case(expr, alts) =>
            helper(expr, bounded) ::: alts.flatMap {
              case Alternative(_, params, expr) =>
                helper(expr, bounded ++ params.map(Open.Var.apply))
            }
          case Open.TypeApp(expr, _) => helper(expr, bounded)
          case Open.LocApp(expr, _)  => helper(expr, bounded)
          case Open.Tup(exprs) =>
            exprs.flatMap { e =>
              helper(e, bounded)
            }
          case Open.Prim(_, args) =>
            args.flatMap { a =>
              helper(a, bounded)
            }
          case Open.Lit(_)                  => Nil
          case Open.Constructor(_, _, expr) => helper(expr, bounded)
          // FIXME: more than 1 abstraction possible?
          case Abs(List((name, _, _)), expr) =>
            helper(expr, bounded + Open.Var(name))
        }
      }
      helper(interTerm, Set.empty)
    }
  }

  object Closed extends ExprData {
    case class Case(expr: Expr, alts: List[Alternative[Closed.Expr]])
        extends Expr
    case class Let(binding: Declaration.Binding[Closed.Expr], expr: Expr)
        extends Expr
    @JsonCodec case class LamRef(id: Int, loc: Location) extends Expr
    @JsonCodec case class LibRef(name: String) extends Expr
    case class ClosedLam(id: Int,
                         body: Closed.Expr,
                         boundedVar: Closed.Var,
                         freeVars: Seq[Closed.Var])

    type LamStore = Map[LamRef, Closed.ClosedLam]

    def compileForInterpreter(typedTerm: Open.Expr): (Closed.Expr, LamStore) = {
      def helper(id: Int,
                 typedTerm: Open.Expr): (Int, Closed.Expr, LamStore) = {
        def buildExprList(list: List[Open.Expr]) = {
          list.foldLeft((Map.empty[LamRef, ClosedLam], id, List.empty[Expr])) {
            case ((eLs, nId, acc), exp) =>
              val (id, eC, ls) = helper(nId, exp)
              (eLs ++ ls, id, acc :+ eC)
          }
        }

        typedTerm match {
          case Open.App(fun, param, l) =>
            val (fId, fC, fStore) = helper(id, fun)
            val (pId, pC, pStore) = helper(fId, param)
            (pId, Closed.App(fC, pC, l), fStore ++ pStore)
          case Open.TypeAbs(_, expr) => helper(id, expr)
          case Open.LocAbs(_, expr)  => helper(id, expr)
          case Open.Let(Declaration.Binding(n, t, bExpr), expr) =>
            val (bId, bC, bS) = helper(id, bExpr)
            val (eId, eC, eS) = helper(bId, expr)
            val let           = Closed.Let(Declaration.Binding(n, t, bC), eC)
            (eId, let, bS ++ eS)
          case Open.Case(expr, alts) =>
            val (eId, eC, eS) = helper(id, expr)
            val (ls, finalId, altsC) =
              alts.foldLeft(
                (eS, eId, List.empty[Alternative[Expr.Closed.Expr]])) {
                case ((store, accId, acc), Alternative(n, p, expr)) =>
                  val (i, e, ls) = helper(accId, expr)
                  (ls ++ store, i, acc :+ Alternative(n, p, e))
              }
            (finalId, Closed.Case(eC, altsC), ls)
          case Open.TypeApp(expr, tpes) =>
            val (i, eC, ls) = helper(id, expr)
            (i, Closed.TypeApp(eC, tpes), ls)
          case Open.LocApp(expr, locs) =>
            val (i, eC, ls) = helper(id, expr)
            (i, Closed.LocApp(eC, locs), ls)
          case Open.Tup(list) =>
            val (nLs, nId, nList) = buildExprList(list)
            (nId, Closed.Tup(nList), nLs)
          case Open.Prim(op, args) =>
            val (ls, id, nargs) = buildExprList(args)
            (id, Closed.Prim(op, nargs), ls)
          case Open.Lit(literal) => (id, Closed.Lit(literal), Map.empty)
          case Open.Constructor(name, tpes, expr) =>
            val (i, c, ls) = helper(id, expr)
            (i, Closed.Constructor(name, tpes, c), ls)
          case Open.Var(name) => (id, Closed.Var(name), Map.empty)
          // FIXME: multiple abstractions
          case Open.Abs(List((name, _, newLoc)), expr) =>
            val ref = LamRef(id, newLoc)
            val (newId, newBody, lamStore) =
              helper(id + 1, expr)
            val closedLam = ClosedLam(
              id,
              newBody,
              Closed.Var(name),
              Open.freeVariables(typedTerm).map(x => Closed.Var(x.name)))
            (newId, ref, (lamStore + (ref -> closedLam)))
        }
      }

      val (_, term, store) = helper(0, typedTerm)
      (term, store)
    }
  }
}
