package rpc

import cats.effect.Async
import cats.implicits._
import io.circe.generic.JsonCodec
import rpc.Declaration.TopLevel
import rpc.Expr.Closed.{LamRef, LamStore}
import rpc.Expr.{Closed, Open}
import rpc.Value._

import scala.collection.mutable

object Interpreter {
  type TierApp[F[_]] = (LamStore,
                        Map[Closed.Var, Closed.Expr],
                        Location,
                        LamRef,
                        Closed.Expr) => F[Closed.Expr]

  @JsonCodec
  case class CallInfo(lamRef: LamRef, bound: Value, frees: Seq[Value])
  case class ExternalCall(callInfo: CallInfo, cont: Cont[Value])

  type Cont[A] = A => Either[ExternalCall, Value]
  type TApp = (LamStore,
               Map[Closed.Var, Value],
               Location,
               LamRef,
               Value,
               Cont[Value]) => Either[ExternalCall, Value]

  case class RequestReplyF[F[_]: Async](
      requestF: CallInfo => F[Either[CallInfo, Value]],
      replyF: Value => F[Either[CallInfo, Value]])

  def processDeclaration[F[_]: Async](decl: TopLevel,
                                      env: Map[Closed.Var, Value])(
      asyncFuns: LamStore => RequestReplyF[F]): F[Map[Closed.Var, Value]] = {
    decl match {
      case TopLevel.Library(name, _) =>
        val lr = Closed.LibRef(name)
        Async[F].pure(env + (Closed.Var(name) -> Closure(Left(lr), env)))
      case TopLevel.DataType(_) => Async[F].pure(env)
      case b: TopLevel.Binding[Open.Expr] =>
        val (TopLevel.Binding(Declaration.Binding(name, _, expr)), store) =
          Declaration.TopLevel.compileBinding(b)

        val RequestReplyF(requestF, replyF) = asyncFuns(store)
        val valueF                          = runClient(expr, store, env)(requestF)(replyF)
        valueF.map { v =>
          env + (Closed.Var(name) -> v)
        }
    }
  }

  def runClient[F[_]: Async](term: Closed.Expr,
                             store: LamStore,
                             env: Map[Closed.Var, Value])(
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

            val continueCall: CallInfo => F[Value] = {
              case CallInfo(lr, bound, frees) => {
                store.get(lr) match {
                  case Some(Closed.ClosedLam(_, body, boundedVar, freeVars)) =>
                    val newEnv = freeVars
                      .zip(frees)
                      .toMap + (boundedVar -> bound)
                    val result =
                      runClient(body, store, newEnv)(requestF)(replyF)
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
      cpsInterpretGeneral(term, store, env, Location.client)(x => Right(x))
    interpretRequestOrValue(valueOrExternalCall)
  }

  private[rpc] def cpsInterpretGeneral(term: Closed.Expr,
                                       store: LamStore,
                                       env: Map[Closed.Var, Value],
                                       localLoc: Location.Loc)(
      cont: Cont[Value]): Either[ExternalCall, Value] = {
    term match {
      case Closed.TypeAbs(strs, expr) => ???
      case Closed.LocAbs(strs, expr)  => ???
      case Closed.TypeApp(expr, tpes) => ???
      case Closed.LocApp(expr, locs) =>
        cpsInterpretGeneral(term, store, env, localLoc)(cont)
      case Closed.Tup(list)                     => ???
      case Closed.Prim(op, args)                => ???
      case Closed.Constructor(name, tpes, expr) => ???
      case Closed.Case(expr, alts)              => ???
      case Closed.Let(binding, expr)            => ???
      // Simple lambda calculus
      case Closed.Lit(literal)      => cont(Constant(literal))
      case v @ Closed.Var(_)        => cont(env(v))
      case lr @ Closed.LamRef(_, _) => cont(Closure(Right(lr), env))
      case Closed.App(fun, param, Some(loc)) =>
        cpsInterpretGeneral(fun, store, env, localLoc) {
          case Closure(ref, closedEnv) =>
            cpsInterpretGeneral(param, store, env, localLoc) { value => // evaluate param
              ref match {
                case Right(lr) =>
                  val Closed.ClosedLam(_, body, bounded, free) = store(lr)
                  if (loc == localLoc)
                    cpsInterpretGeneral(body,
                                        store,
                                        closedEnv + (bounded -> value),
                                        localLoc)(cont)
                  else
                    Left(
                      ExternalCall(CallInfo(lr, value, free.map(closedEnv)),
                                   cont))
                case Left(Closed.LibRef(name)) =>
                  (Lib: LibInt).get(name) match {
                    case None =>
                      throw new RuntimeException(
                        s"$name is not a library function")
                    case Some(f) => Right(f(List(value)))
                  }
              }
            }
        }
    }
  }

  def performServerRequest(store: LamStore,
                           callInfo: CallInfo): Either[ExternalCall, Value] = {
    val CallInfo(lr, bound, frees) = callInfo
    store.get(lr) match {
      case Some(Closed.ClosedLam(_, body, boundedVar, freeVars)) =>
        val newEnv = freeVars.zip(frees).toMap + (boundedVar -> bound)
        Interpreter.cpsInterpretGeneral(body, store, newEnv, Location.server)(
          Right.apply)
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
