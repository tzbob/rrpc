package rpc

import rpc.InterTerm.{ClosedLam, LamRef}
import cats.effect.Async
import cats.implicits._

import scala.annotation.tailrec

object Interpreter {
  case class DeferToServer(lamRef: LamRef, variables: Seq[InterTerm])
  case class CallToClient(lamRef: LamRef, variables: Seq[InterTerm])

  type TierApp[F[_]] = (LamStore,
                        Map[InterTerm.Var, InterTerm],
//                        DeferToServer => F[InterTerm],
                        TypedLocation,
                        LamRef,
                        InterTerm) => F[InterTerm]

  type LamStore = Map[LamRef, InterTerm.ClosedLam]

  def compileForInterpreter(typedTerm: TypedTerm): (InterTerm, LamStore) = {
    def helper(id: Int,
               location: Location,
               typedTerm: TypedTerm): (Int, InterTerm, LamStore) = {
      typedTerm match {
        case TypedTerm.Const(i)  => (id, InterTerm.Const(i), Map.empty)
        case TypedTerm.Var(name) => (id, InterTerm.Var(name), Map.empty)
        case TypedTerm.Lam(newLoc, name, _, body) =>
          val ref                        = LamRef(id, newLoc)
          val (newId, newBody, lamStore) = helper(id + 1, newLoc, body)
          val closedLam = ClosedLam(
            id,
            newBody,
            InterTerm.Var(name),
            TypedTerm.freeVariables(typedTerm).map(InterTerm.Var.fromTVar))
          (newId, ref, (lamStore + (ref -> closedLam)))
        case TypedTerm.App(l, fun, param) =>
          val (newId, funC, funStore)       = helper(id, location, fun)
          val (newerId, paramC, paramStore) = helper(newId, location, param)
          (newerId, InterTerm.App(l, funC, paramC), (funStore ++ paramStore))
      }
    }

    val (_, term, store) = helper(0, Location.Client, typedTerm)
    (term, store)
  }

  def runClient[F[_]: Async](term: Term)(
      request: DeferToServer => F[InterTerm]): F[InterTerm] =
    runClient(Infer.infer(term))(request)

  def runClient[F[_]: Async](typedTerm: TypedTerm)(
      request: DeferToServer => F[InterTerm]): F[InterTerm] = {
    val (term, store) = compileForInterpreter(typedTerm)
    runClient(term, store)(request)
  }

  def runClient[F[_]: Async](term: InterTerm, store: LamStore)(
      request: DeferToServer => F[InterTerm]): F[InterTerm] = {
    interpretGeneral(term, store, Map.empty)(appClient[F](request))
  }

  private def interpretGeneral[F[_]: Async](
      term: InterTerm,
      store: LamStore,
      env: Map[InterTerm.Var, InterTerm])(tierApp: TierApp[F]): F[InterTerm] = {
    term match {
      case v @ InterTerm.Var(_) => Async[F].pure(env(v))
      case InterTerm.App(loc, fun, param) =>
        Async[F].flatMap(interpretGeneral(fun, store, env)(tierApp)) {
          case lr @ InterTerm.LamRef(_, _) =>
            Async[F].flatMap(interpretGeneral(param, store, env)(tierApp)) {
              value =>
                tierApp(store, env, loc, lr, value)
            }
          case x => throw new RuntimeException(s"Cannot apply to $x")
        }

      case InterTerm.Const(_) => Async[F].pure(term)
      case lr @ LamRef(_, _)  => Async[F].pure(lr)
    }
  }

  private def appClient[F[_]: Async](
      request: DeferToServer => F[InterTerm]): TierApp[F] =
    (store, env, loc, lr, value) => {
      val ClosedLam(_, body, bounded, free) = store(lr)
      loc match {
        case TypedLocation.client =>
          interpretGeneral(body, store, env + (bounded -> value))(
            appClient[F](request))
        case TypedLocation.server =>
          // Wait for response from server,
          // but this response may not be the result, it might be CALL
          request(DeferToServer(lr, value +: free.map(env)))
        case _ =>
          throw new RuntimeException(s"No location variables allowed: $loc")
      }
    }

  def runServer(term: InterTerm, store: LamStore) = {
    // requestResponder + support for Call?
    //
  }

  private def appServer[F[_]: Async]: TierApp[F] =
    (store, env, loc, lr, value) => {
      val ClosedLam(_, body, bounded, free) = store(lr)
      loc match {
        case TypedLocation.client => ???
        case TypedLocation.server =>
          interpretGeneral(body, store, env + (bounded -> value))(appServer[F])
      }
    }

  def requestResponder[F[_]: Async](
      store: LamStore): (DeferToServer => F[InterTerm]) = deferToServer => {

    val DeferToServer(lamRef, bVal :: fVals) = deferToServer

    store.get(lamRef) match {
      case Some(ClosedLam(_, body, boundedVar, freeVars)) =>
        interpretGeneral(
          body,
          store,
          freeVars.zip(fVals).toMap + (boundedVar -> bVal))(appServer)
      case None => throw new RuntimeException(s"No $lamRef in store $store")
    }
  }
}
