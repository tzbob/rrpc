package rpc

import cats.effect.Async
import cats.implicits._
import io.circe.generic.JsonCodec
import rpc.InterTerm.{ClosedLam, LamRef, LamStore}
import rpc.Value._

import scala.collection.mutable

object Interpreter {
  type TierApp[F[_]] = (LamStore,
                        Map[InterTerm.Var, InterTerm],
                        TypedLocation,
                        LamRef,
                        InterTerm) => F[InterTerm]

  @JsonCodec
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
    runClient(term, store, Map.empty) { info =>
      request(info).map(x => Right.apply(x): Either[CallInfo, Value])
    }(???)

  def runClient[F[_]: Async](term: InterTerm,
                             store: LamStore,
                             env: Map[InterTerm.Var, Value])(
      requestF: CallInfo => F[Either[CallInfo, Value]])(
      replyF: Value => F[Either[CallInfo, Value]]): F[Value] = {
    def interpretRequestOrValue(
        reqOrVal: Either[ExternalCall, Value]): F[Value] = {
      val performExternalCall: ExternalCall => F[Value] = {
        case ExternalCall(callInfo, cont) =>
          def interpretResponseF(
              responseF: F[Either[CallInfo, Value]]): F[Value] = {
            val continueValue: Value => F[Value] =
              interpretRequestOrValue _ compose cont

            // FIXME: THIS SHOULD MAKE A REQUEST BACK WITH AN ANSWER
            val continueCall: CallInfo => F[Value] = {
              case CallInfo(lr, bound, frees) => {
                store.get(lr) match {
                  case Some(ClosedLam(_, body, boundedVar, freeVars)) =>
                    val newEnv = freeVars
                      .zip(frees)
                      .toMap + (boundedVar -> bound)
                    val result =
                      runClient(body, store, newEnv)(requestF)(replyF)
                    // FIXME: Request on this result (completes Server's request)
                    val responseF = Async[F].flatMap(result)(replyF)
                    interpretResponseF(responseF)
                  case None =>
                    throw new RuntimeException(s"No $lr in store $store")
                }
              }
            }
            responseF.map(_.fold(continueCall, continueValue)).flatten
          }
          interpretResponseF(requestF(callInfo))
      }

      val result: Either[Value, F[Value]] =
        reqOrVal.swap.map(performExternalCall)

      result.sequence.map(_.merge)
    }

    val valueOrExternalCall: Either[ExternalCall, Value] =
      cpsInterpretGeneral(term, store, env, TypedLocation.client)(x => Right(x))
    interpretRequestOrValue(valueOrExternalCall)
  }

  private[rpc] def cpsInterpretGeneral(term: InterTerm,
                                       store: LamStore,
                                       env: Map[InterTerm.Var, Value],
                                       localLoc: TypedLocation.Location)(
      cont: Cont[Value]): Either[ExternalCall, Value] = {
    term match {
      case v @ InterTerm.Var(_) => cont(env(v))
      case InterTerm.Const(i)   => cont(Const(i))
      case lr @ LamRef(_, _)    => cont(Closure(lr, env))

      case InterTerm.App(loc, fun, param) =>
        cpsInterpretGeneral(fun, store, env, localLoc) {
          case Closure(lr, closedEnv) =>
            cpsInterpretGeneral(param, store, env, localLoc) { value => // evaluate param
              val ClosedLam(_, body, bounded, free) = store(lr)
              if (loc == localLoc)
                cpsInterpretGeneral(body,
                                    store,
                                    closedEnv + (bounded -> value),
                                    localLoc)(cont)
              else
                Left(
                  ExternalCall(CallInfo(lr, value, free.map(closedEnv)), cont))
            }
        }
    }
  }

  def performServerRequest(store: LamStore,
                           callInfo: CallInfo): Either[ExternalCall, Value] = {
    val CallInfo(lr, bound, frees) = callInfo
    store.get(lr) match {
      case Some(ClosedLam(_, body, boundedVar, freeVars)) =>
        val newEnv = freeVars.zip(frees).toMap + (boundedVar -> bound)
        Interpreter.cpsInterpretGeneral(body,
                                        store,
                                        newEnv,
                                        TypedLocation.server)(Right.apply)
      case None =>
        throw new RuntimeException(s"No $lr in store $store")
    }
  }

  def handleClientResponse[F[_]: Async](
      store: LamStore,
      value: Value,
      queue: mutable.Queue[Cont[Value]]): Either[ExternalCall, Value] = {
    val cont = queue.dequeue()
    cont(value)
  }
}
