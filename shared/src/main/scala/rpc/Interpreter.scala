package rpc

import cats.effect.Async
import cats.implicits._
import io.circe.generic.JsonCodec
import rpc.Declaration.TopLevel
import rpc.Expr.Closed.{LamRef, LamStore, LibRef}
import rpc.Expr.{Closed, Open}
import rpc.Value._
import rpc.error.{CaseError, MissingLibError}

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
               Env,
               Location,
               LamRef,
               Value,
               Cont[Value]) => Either[ExternalCall, Value]

  case class RequestReplyF[F[_]: Async](
      requestF: CallInfo => F[Either[CallInfo, Value]],
      replyF: Value => F[Either[CallInfo, Value]])

  def processDeclaration[F[_]: Async](decl: TopLevel, env: Env)(
      asyncFuns: LamStore => RequestReplyF[F]): F[Env] = {
    decl match {
      case TopLevel.Library(name, _) =>
        val lr = Closed.LibRef(name)
        Async[F].pure(env.add(name, Closure(Left(lr), env)))
      case TopLevel.DataType(_) => Async[F].pure(env)
      case b @ TopLevel.Binding(_) =>
        val (TopLevel.Binding(Declaration.Binding(name, _, expr)), store) =
          Declaration.TopLevel.compileBinding(
            b.asInstanceOf[TopLevel.Binding[Open.Expr]],
            env.lamStore)

        val RequestReplyF(requestF, replyF) = asyncFuns(store)
        val valueF =
          runClient(expr, store, env.add(name, expr))(requestF)(replyF)
        valueF.map { v =>
          env.add(name, v).mergeStore(store)
        }
    }
  }

  // FIXME: Document this entire thing
  def runClient[F[_]: Async](term: Closed.Expr, store: LamStore, env: Env)(
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
                    val newValueEnv = freeVars
                      .map(_.name)
                      .zip(frees)
                      .toMap + (boundedVar.name -> bound)
                    val newEnv = env.copy(values = newValueEnv)
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
                                       env: Env,
                                       localLoc: Location.Loc)(
      cont: Cont[Value]): Either[ExternalCall, Value] = {
    def combinedCpsInterpret(results: List[Closed.Expr])(
        cont: Cont[List[Value]]): Either[ExternalCall, Value] = {
      def helper(values: List[Value], results: List[Closed.Expr])(
          cont: Cont[List[Value]]): Either[ExternalCall, Value] =
        results match {
          case Nil => cont(values.reverse)
          case e :: es =>
            cpsInterpretGeneral(e, store, env, localLoc) { v =>
              helper(v :: values, es)(cont)
            }
        }
      helper(Nil, results)(cont)
    }

    term match {
      case Closed.TypeAbs(strs, expr) => cont(TpeClosure(strs, expr, env))
      case Closed.LocAbs(strs, expr)  => cont(LocClosure(strs, expr, env))

      case Closed.TypeApp(expr, tpes) =>
        cpsInterpretGeneral(expr, store, env, localLoc) {
          case TpeClosure(abs, body, _) =>
            val extendedEnv = abs.zip(tpes).foldLeft(env) {
              case (acc, (name, tpe)) =>
                acc.add(name, tpe)
            }
            cpsInterpretGeneral(body, store, extendedEnv, localLoc)(cont)
          case e => cont(e)
        }
      case Closed.LocApp(expr, locs) =>
        cpsInterpretGeneral(expr, store, env, localLoc) {
          case LocClosure(abs, body, _) =>
            val extendedEnv = abs.zip(locs).foldLeft(env) {
              case (acc, (name, loc)) =>
                acc.add(name, loc)
            }
            cpsInterpretGeneral(body, store, extendedEnv, localLoc)(cont)
          case e => cont(e)
        }
      case Closed.Tup(exprs) =>
        combinedCpsInterpret(exprs) { values =>
          cont(Value.Tupled(values))
        }
      case Closed.Prim(op, args) =>
        combinedCpsInterpret(args) { values =>
          val constants = values.map { case Value.Constant(lit) => lit }
          cont(Value.Constant(Operator.operators(op)(constants)))
        }

      case Closed.Constructor(name, _, exprs) =>
        combinedCpsInterpret(exprs) { values =>
          cont(Value.Constructed(name, values))
        }
      case Closed.Case(expr, alts) =>
        cpsInterpretGeneral(expr, store, env, localLoc) {
          case v @ Value.Constructed(tag, args) =>
            alts.find(_.name == tag) match {
              case Some(Alternative(_, params, body)) =>
                val newEnv = params.zip(args).foldLeft(env) {
                  case (envAcc, (k, v)) =>
                    envAcc.add(k, v)
                }
                cpsInterpretGeneral(body, store, newEnv, localLoc)(cont)
              case None => throw CaseError(v, alts)
            }
        }

      case Closed.Let(bindings, expr) =>
        combinedCpsInterpret(bindings.map(_.expr)) { values =>
          val newEnv = bindings.zip(values).foldLeft(env) {
            case (accEnv, (b, value)) =>
              accEnv.add(b.name, value)
          }
          cpsInterpretGeneral(expr, store, newEnv, localLoc)(cont)
        }

      // Simple lambda calculus
      case Closed.Lit(literal) => cont(Constant(literal))
      case Closed.Var(name) =>
        env.value(name) match {
          case None =>
            env.recurse(name) match {
              case None => throw new RuntimeException(s"$name not found.")
              case Some(expr) =>
                cpsInterpretGeneral(expr, store, env, localLoc)(cont)
            }
          case Some(v) => cont(v)
        }
//        cont(env.value(name))

      case lr @ Closed.LamRef(_, _) => cont(Closure(Right(lr), env))
      case lr @ Closed.LibRef(_)    => cont(Closure(Left(lr), env))
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
                                        closedEnv.add(bounded.name, value),
                                        localLoc)(cont)
                  else
                    Left(
                      ExternalCall(CallInfo(lr,
                                            value,
                                            free.map(v =>
                                              closedEnv.value(v.name).get)),
                                   cont))
                case Left(Closed.LibRef(name)) =>
                  (Lib: LibInt).get(name) match {
                    case None    => throw MissingLibError(name)
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
        val newVEnv = freeVars
          .map(_.name)
          .zip(frees)
          .toMap + (boundedVar.name -> bound)
        Interpreter.cpsInterpretGeneral(body,
                                        store,
                                        Env.empty.copy(values = newVEnv),
                                        Location.server)(Right.apply)
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
