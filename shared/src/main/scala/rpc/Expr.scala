package rpc

import cats.syntax.functor._
import io.circe.generic.JsonCodec
import io.circe.{
  Decoder,
  DecodingFailure,
  Encoder,
  HCursor,
  KeyDecoder,
  KeyEncoder
}
object Expr {

  object Open {
    sealed trait Expr

    case class Var(name: String) extends Expr
    case class App(fun: Expr, param: Expr, location: Option[Location])
        extends Expr
    case class TypeAbs(strs: List[String], expr: Expr)  extends Expr
    case class LocAbs(strs: List[String], expr: Expr)   extends Expr
    case class TypeApp(expr: Expr, tpes: List[Tpe])     extends Expr
    case class LocApp(expr: Expr, locs: List[Location]) extends Expr
    case class Tup(list: List[Expr])                    extends Expr
    case class Prim(op: Operator, args: List[Expr])     extends Expr
    case class Lit(literal: Literal)                    extends Expr
    case class Constructor(name: String, tpes: List[Tpe], expr: List[Expr])
        extends Expr
    case class Case(expr: Expr, alts: List[Alternative[Open.Expr]]) extends Expr
    case class Let(bindings: List[Declaration.Binding[Open.Expr]], expr: Expr)
        extends Expr
    case class Abs(abss: List[(String, Tpe, Location)], expr: Expr) extends Expr

    private implicit val prioritizedLocationDecoder: Decoder[Location] =
      Location.locD
    private implicit val prioritizedOpDecoder: Decoder[Operator] =
      Operator.opDImpl

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
        .as[(String, List[Tpe], List[Expr])]
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

    def freeVariables(interTerm: Open.Expr): List[Open.Var] = {
      def helper(expr: Open.Expr, bounded: Set[Open.Var]): List[Open.Var] = {
        expr match {
          case v @ Open.Var(_) => if (bounded contains v) Nil else List(v)
          case Open.App(a, b, _) =>
            helper(a, bounded) ::: helper(b, bounded)
          case Open.TypeAbs(_, expr) => helper(expr, bounded)
          case Open.LocAbs(_, expr)  => helper(expr, bounded)
          case Open.Let(bindings, expr) =>
            val frees = bindings.flatMap {
              case Declaration.Binding(_, _, exprB) =>
                helper(exprB, bounded)
            }
            val newBounded = bindings.foldLeft(bounded) { (acc, b) =>
              acc + Var(b.name)
            }
            frees ::: helper(expr, newBounded)
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
          case Open.Lit(_) => Nil
          case Open.Constructor(_, _, exprs) =>
            exprs.flatMap { expr =>
              helper(expr, bounded)
            }
          case Abs(List((name, _, _)), expr) =>
            helper(expr, bounded + Open.Var(name))
        }
      }
      helper(interTerm, Set.empty)
    }
  }

  object Closed {
    sealed trait Expr
    case class Var(name: String) extends Expr
    case class App(fun: Expr, param: Expr, location: Option[Location])
        extends Expr
    case class TypeAbs(strs: List[String], expr: Expr)  extends Expr
    case class LocAbs(strs: List[String], expr: Expr)   extends Expr
    case class TypeApp(expr: Expr, tpes: List[Tpe])     extends Expr
    case class LocApp(expr: Expr, locs: List[Location]) extends Expr
    case class Tup(list: List[Expr])                    extends Expr
    case class Prim(op: Operator, args: List[Expr])     extends Expr
    case class Lit(literal: Literal)                    extends Expr
    case class Constructor(name: String, tpes: List[Tpe], expr: List[Expr])
        extends Expr
    case class Case(expr: Expr, alts: List[Alternative[Closed.Expr]])
        extends Expr
    case class Let(binding: List[Declaration.Binding[Closed.Expr]], expr: Expr)
        extends Expr
    @JsonCodec case class LamRef(id: Int, loc: Location) extends Expr
    @JsonCodec case class LibRef(name: String)           extends Expr
    case class ClosedLam(id: Int,
                         body: Closed.Expr,
                         boundedVar: Closed.Var,
                         freeVars: List[Closed.Var])

    type LamStore = Map[LamRef, Closed.ClosedLam]

    import io.circe.generic.auto._
    import io.circe.syntax._
    import io.circe.parser.decode
    implicit val keyLamRefEncoder = new KeyEncoder[Closed.LamRef] {
      override def apply(key: Closed.LamRef): String = key.asJson.noSpaces
    }

    implicit val keyLamRefDecoder = new KeyDecoder[Closed.LamRef] {
      override def apply(key: String): Option[Closed.LamRef] =
        decode[Closed.LamRef](key).toOption
    }

    def compileForInterpreter(typedTerm: Open.Expr,
                              startStore: LamStore): (Closed.Expr, LamStore) = {
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
          case Open.TypeAbs(strs, expr) =>
            val (eId, eC, eStore) = helper(id, expr)
            (eId, Closed.TypeAbs(strs, eC), eStore)
          case Open.LocAbs(strs, expr) =>
            val (eId, eC, eStore) = helper(id, expr)
            (eId, Closed.LocAbs(strs, eC), eStore)
          case Open.Let(bindings, expr) =>
            val (nLs, nId, nList) = buildExprList(bindings.map(_.expr))
            val newBindings =
              nList.zip(bindings).map { case (e, b) => b.copy(expr = e) }

            val (eId, eC, eS) = helper(nId, expr)
            val let           = Closed.Let(newBindings, eC)

            (eId, let, nLs ++ eS)
          case Open.Case(expr, alts) =>
            val (ls, altId, altExprs) = buildExprList(alts.map(_.expr))
            val (eId, eC, eS)         = helper(altId, expr)
            val newAlts =
              altExprs.zip(alts).map { case (e, a) => a.copy(expr = e) }
            (eId, Closed.Case(eC, newAlts), ls ++ eS)
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
          case Open.Constructor(name, tpes, exprs) =>
            val (ls, id, nExprs) = buildExprList(exprs)
            (id, Closed.Constructor(name, tpes, nExprs), ls)
          case Open.Var(name) => (id, Closed.Var(name), Map.empty)
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

      val startId =
        startStore.keys.map(_.id).toList.sorted.lastOption.getOrElse(0)
      val (_, term, store) = helper(startId, typedTerm)
      (term, startStore ++ store)
    }
  }
}
