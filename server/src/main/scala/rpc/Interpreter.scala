package rpc

import rpc.InterTerm.{ClosedLam, LamRef, LamStore}
import cats.effect.Async
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.immutable.ListSet
import rpc.Value._

object Interpreter {
  case class CallToClient(lamRef: LamRef, variables: Seq[InterTerm])

  type TierApp[F[_]] = (LamStore,
                        Map[InterTerm.Var, InterTerm],
                        TypedLocation,
                        LamRef,
                        InterTerm) => F[InterTerm]

  case class CallInfo(lamRef: LamRef, bound: Value, frees: Seq[Value])
  case class ExternalCall(callInfo: CallInfo, cont: Cont[Value])

  type Cont[A] = A => Either[ExternalCall, Value]
  type TApp = (LamStore,
               Map[InterTerm.Var, Value],
               TypedLocation,
               LamRef,
               Value,
               Cont[Value]) => Either[ExternalCall, Value]

  def runClient[F[_]: Async](term: Term)(
      request: CallInfo => F[Value]): F[Value] =
    runClient(Infer.infer(term))(request)

  def runClient[F[_]: Async](typedTerm: TypedTerm)(
      request: CallInfo => F[Value]): F[Value] = {
    val (term, store) = InterTerm.compileForInterpreter(typedTerm)
    runClient(term, store)(request)
  }

  def runClient[F[_]: Async](term: InterTerm, store: LamStore)(
      request: CallInfo => F[Value]): F[Value] =
    cleanImpl(term, store, Map.empty) { info =>
      request(info).map(Right.apply)
    }

  def cleanImpl[F[_]: Async](term: InterTerm,
                             store: LamStore,
                             env: Map[InterTerm.Var, Value])(
      requestF: CallInfo => F[Either[CallInfo, Value]]): F[Value] = {
    def interpretRequestOrValue(
        reqOrVal: Either[ExternalCall, Value]): F[Value] = {
      val performExternalCall: ExternalCall => F[Value] = {
        case ExternalCall(callInfo, cont) =>
          val responseF: F[Either[CallInfo, Value]] = requestF(callInfo)
          val continueValue: Value => F[Value] =
            interpretRequestOrValue _ compose cont

          val continueCall: CallInfo => F[Value] = {
            case CallInfo(lr, bound, frees) => {
              store.get(lr) match {
                case Some(ClosedLam(_, body, boundedVar, freeVars)) =>
                  val newEnv = freeVars.zip(frees).toMap + (boundedVar -> bound)
                  cleanImpl(body, store, newEnv)(requestF)
                case None =>
                  throw new RuntimeException(s"No $lr in store $store")
              }
            }
          }
          responseF.map(_.fold(continueCall, continueValue)).flatten
      }

      val result: Either[Value, F[Value]] =
        reqOrVal.swap.map(performExternalCall)

      result.sequence.map(_.merge)
    }

    val valueOrExternalCall: Either[ExternalCall, Value] =
      cpsInterpretGeneral(term, store, env)(clientTApp())(x => Right(x))
    interpretRequestOrValue(valueOrExternalCall)
  }

  def clientTApp(): TApp = (store, env, loc, lr, value, cont) => {
    val ClosedLam(_, body, bounded, free) = store(lr)
    loc match {
      case TypedLocation.client =>
        cpsInterpretGeneral(body, store, env + (bounded -> value))(
          clientTApp())(cont)
      case TypedLocation.server =>
        Left(ExternalCall(CallInfo(lr, value, free.map(env)), cont))
      case _ =>
        throw new RuntimeException(s"No location variables allowed: $loc")
    }
  }

  private def cpsInterpretGeneral(term: InterTerm,
                                  store: LamStore,
                                  env: Map[InterTerm.Var, Value])(tApp: TApp)(
      cont: Cont[Value]): Either[ExternalCall, Value] = {
    term match {
      case v @ InterTerm.Var(_, _) => cont(env(v))
      case InterTerm.Const(i)      => cont(Const(i))
      case lr @ LamRef(_, _)       => cont(Closure(lr, env))

      case InterTerm.App(loc, fun, param) =>
        cpsInterpretGeneral(fun, store, env)(tApp) { // evaluate to lamref
          case Closure(lr, closedEnv) =>
            cpsInterpretGeneral(param, store, env)(tApp) { value => // evaluate param
              tApp(store, closedEnv, loc, lr, value, cont)
            }
        }
    }
  }

//  def runServer(term: InterTerm, store: LamStore) = {
//    // requestResponder + support for Call?
//    //
//  }
//
//  private def appServer[F[_]: Async]: TierApp[F] =
//    (store, env, loc, lr, value) => {
//      val ClosedLam(_, body, bounded, free) = store(lr)
//      loc match {
//        case TypedLocation.client => ???
//        case TypedLocation.server =>
//          interpretGeneral(body, store, env + (bounded -> value))(appServer[F])
//      }
//    }

//  def requestResponder[F[_]: Async](
//      store: LamStore): (DeferToServer => F[InterTerm]) = deferToServer => {
//
//    val DeferToServer(lamRef, bVal :: fVals) = deferToServer
//
//    store.get(lamRef) match {
//      case Some(ClosedLam(_, body, boundedVar, freeVars)) =>
//        interpretGeneral(
//          body,
//          store,
//          freeVars.zip(fVals).toMap + (boundedVar -> bVal))(appServer)
//      case None => throw new RuntimeException(s"No $lamRef in store $store")
//    }
//  }
}
