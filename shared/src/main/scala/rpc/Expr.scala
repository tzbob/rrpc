package rpc

import cats.syntax.functor._
import io.circe.{
  Decoder,
  DecodingFailure,
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
    case class Constructor(name: String,
                           locs: List[Location],
                           tpes: List[Tpe],
                           expr: List[Expr])
        extends Expr
    case class Case(expr: Expr, alts: List[Alternative[Open.Expr]]) extends Expr
    case class Let(bindings: List[Declaration.Binding[Open.Expr]], expr: Expr)
        extends Expr
    case class Abs(abss: List[(String, Tpe, Location)], expr: Expr) extends Expr

    // Added for native libs
    case class Native(name: String, vars: List[Var]) extends Expr

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
      c.downField("Tuple").as[List[Expr]].map(Tup)
    private implicit val exprPrimD: Decoder[Prim] = (c: HCursor) =>
      c.downField("Prim").as[(Operator, List[Expr])].map(Prim.tupled)
    private implicit val exprLitD: Decoder[Lit] = (c: HCursor) =>
      c.downField("Lit").as[Literal].map(Lit)
    private implicit val exprDataD: Decoder[Constructor] = (c: HCursor) =>
      c.downField("Constr")
        .as[(String, List[Location], List[Tpe], List[Expr])]
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

    def traversed(expr: Open.Expr): List[Open.Expr] = {
      expr :: (expr match {
        case Open.Var(_) => Nil
        case Open.App(a, b, _) =>
          traversed(a) ::: traversed(b)
        case Open.TypeAbs(_, expr) => traversed(expr)
        case Open.LocAbs(_, expr)  => traversed(expr)
        case Open.Let(bindings, expr) =>
          val traversedB = bindings.flatMap {
            case Declaration.Binding(_, _, exprB) => traversed(exprB)
          }
          traversedB ::: traversed(expr)
        case Open.Case(expr, alts) =>
          traversed(expr) ::: alts.flatMap {
            case Alternative.Alt(_, _, expr) => traversed(expr)
            case Alternative.TupAlt(_, expr) => traversed(expr)
          }
        case Open.TypeApp(expr, _)            => traversed(expr)
        case Open.LocApp(expr, _)             => traversed(expr)
        case Open.Tup(exprs)                  => exprs.flatMap(traversed)
        case Open.Prim(_, args)               => args.flatMap(traversed)
        case Open.Lit(_)                      => Nil
        case Open.Constructor(_, _, _, exprs) => exprs.flatMap(traversed)
        case Open.Native(_, vars)             => vars.flatMap(traversed)
        case Abs(List((_, _, _)), expr)       => traversed(expr)
      })
    }

    def freeVariablesAndLocs(
        interTerm: Open.Expr
    ): (List[Location.Var], List[Open.Var]) = {

      def unpackTupleList(a: List[(List[Var], List[Location.Var], List[Var])]) =
        (a.map(_._1).flatten, a.map(_._2).flatten, a.map(_._3).flatten)

      val result = traversed(interTerm).map {
        case v @ Open.Var(_) => (Nil, Nil, List(v))
        case Open.Let(bindings, _) =>
          val b = bindings.map {
            case Declaration.Binding(str, _, _) =>
              val bound = Open.Var(str)
              (List(bound), Nil, List(bound))
          }
          unpackTupleList(b)
        case Open.LocAbs(locs, _) => (Nil, locs.map(Location.Var), Nil)
        case Open.Case(_, alts) =>
          val a = alts.map { alternative =>
            val params = alternative match {
              case Alternative.Alt(_, params, _) => params
              case Alternative.TupAlt(params, _) => params
            }
            val boundVars = params.map(Open.Var)
            (boundVars, Nil, boundVars)
          }
          unpackTupleList(a)
        case Open.Abs(abss, _) =>
          val bound = abss.map { case (name, _, _) => Open.Var(name) }
          (bound, Nil, bound)
        case Open.App(_, _, Some(l @ Location.Var(_))) => (Nil, List(l), Nil)
        case Open.LocApp(_, locs) =>
          (Nil, locs.collect { case l @ Location.Var(_) => l }, Nil)
        case _ => (Nil, Nil, Nil)
      }

      val (bound, locs, vars) = unpackTupleList(result)
      (locs, vars.filterNot(bound.toSet))
    }
  }

  object Closed {
    sealed trait Expr
    case class Var(name: String) extends Expr
    case class App(fun: Expr, param: Expr, location: Option[Location])
        extends Expr
    case class TypeApp(expr: Expr, tpes: List[Tpe])     extends Expr
    case class LocApp(expr: Expr, locs: List[Location]) extends Expr
    case class Tup(list: List[Expr])                    extends Expr
    case class Prim(op: Operator, args: List[Expr])     extends Expr
    case class Lit(literal: Literal)                    extends Expr
    case class Constructor(name: String,
                           locs: List[Location],
                           tpes: List[Tpe],
                           expr: List[Expr])
        extends Expr
    case class Case(expr: Expr, alts: List[Alternative[Closed.Expr]])
        extends Expr
    case class Let(binding: List[Declaration.Binding[Closed.Expr]], expr: Expr)
        extends Expr
    case class Native(name: String, vars: List[Var]) extends Expr
    case class LamRef(id: Int)                       extends Expr
    case class ClosedLam(id: Int,
                         body: Closed.Expr,
                         boundVars: List[Closed.Var],
                         boundTpeVars: List[Tpe.Var],
                         boundLocVars: List[Location.Var],
                         freeVars: List[Closed.Var],
                         tpeVars: List[Tpe.Var],
                         locVars: List[Location.Var])

    object Native {
      def boundVariable(name: String) = Closed.Var(s"${name}_bound%%")
    }

    type LamStore = Map[LamRef, Closed.ClosedLam]

    import io.circe.syntax._
    import io.circe.generic.auto._
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

        def makeClosedLam(boundedVars: List[String],
                          expr: Open.Expr,
                          tpeVars: List[String],
                          locVars: List[String]) = {

          val ref                        = LamRef(id)
          val (newId, newBody, lamStore) = helper(id + 1, expr)
          val (locs, vars)               = Open.freeVariablesAndLocs(typedTerm)

          val closedLam = ClosedLam(
            id,
            newBody,
            boundedVars.map(Expr.Closed.Var),
            tpeVars.map(Tpe.Var),
            locVars.map(Location.Var),
            vars.map(x => Closed.Var(x.name)),
            Nil,
            locs
          )
          (newId, ref, (lamStore + (ref -> closedLam)))
        }

        typedTerm match {
          case Open.App(fun, param, l) =>
            val (fId, fC, fStore) = helper(id, fun)
            val (pId, pC, pStore) = helper(fId, param)
            (pId, Closed.App(fC, pC, l), fStore ++ pStore)
          case Open.Let(bindings, expr) =>
            val (nLs, nId, nList) = buildExprList(bindings.map(_.expr))
            val newBindings =
              nList.zip(bindings).map { case (e, b) => b.copy(expr = e) }

            val (eId, eC, eS) = helper(nId, expr)
            val let           = Closed.Let(newBindings, eC)

            (eId, let, nLs ++ eS)
          case Open.Case(expr, alternatives) =>
            val (ls, altId, altExprs) = alternatives.foldLeft(
              (Map.empty[LamRef, ClosedLam],
               id,
               List.empty[Alternative[Expr.Closed.Expr]])) {
              case ((eLs, nId, acc), alt) =>
                alt match {
                  case a @ Alternative.Alt(_, _, expr) =>
                    val (id, eC, ls) = helper(nId, expr)
                    (eLs ++ ls, id, acc :+ a.copy(expr = eC))
                  case t @ Alternative.TupAlt(_, expr) =>
                    val (id, eC, ls) = helper(nId, expr)
                    (eLs ++ ls, id, acc :+ t.copy(expr = eC))
                }
            }
            val (eId, eC, eS) = helper(altId, expr)
            (eId, Closed.Case(eC, altExprs), ls ++ eS)
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
          case Open.Constructor(name, locs, tpes, exprs) =>
            val (ls, id, nExprs) = buildExprList(exprs)
            (id, Closed.Constructor(name, locs, tpes, nExprs), ls)
          case Open.Var(name) => (id, Closed.Var(name), Map.empty)
          case Open.Native(name, vars) =>
            (id,
             Closed.Native(name, vars.map(v => Closed.Var(v.name))),
             Map.empty)

          case Open.Abs(List((name, _, _)), expr) =>
            makeClosedLam(List(name), expr, Nil, Nil)
          case Open.TypeAbs(strs, expr) => makeClosedLam(Nil, expr, strs, Nil)
          case Open.LocAbs(strs, expr)  => makeClosedLam(Nil, expr, Nil, strs)
        }
      }

      val startId          = LamStore.newRef(startStore).id
      val (_, term, store) = helper(startId, typedTerm)
      (term, startStore ++ store)
    }
  }
}
