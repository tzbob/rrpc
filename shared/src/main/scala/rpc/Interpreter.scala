package rpc

import cats.effect.Async
import cats.implicits._
import io.circe.generic.JsonCodec
import rpc.Declaration.TopLevel
import rpc.Expr.Closed.{ClosedLam, LamRef, LamStore}
import rpc.Expr.{Closed, Open}
import rpc.Value._
import rpc.error.{CaseError, MissingLibError, MissingValueError}

import scala.collection.mutable

object Interpreter {
  type TierApp[F[_]] = (LamStore,
                        Map[Closed.Var, Closed.Expr],
                        Location,
                        LamRef,
                        Closed.Expr) => F[Closed.Expr]

  // FIXME: Make Env a minimal subset using freelocs/freevars!
  @JsonCodec
  case class CallInfo(lamRef: LamRef, bound: Value, env: Env)
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

  def runDeclarations[F[_]: Async](decls: List[TopLevel])(
      asyncFuns: LamStore => RequestReplyF[F]): F[(Env, LamStore)] = {
    decls.foldLeft(Async[F].pure(Env.empty -> LamStore.empty)) { (accIO, dec) =>
      accIO.flatMap {
        case (acc, store) =>
          Interpreter.processDeclaration(dec, acc, store)(asyncFuns)
      }
    }
  }

  def processDeclaration[F[_]: Async](decl: TopLevel,
                                      env: Env,
                                      store: LamStore)(
      asyncFuns: LamStore => RequestReplyF[F]): F[(Env, LamStore)] = {
    decl match {
      case TopLevel.Library(name, _) =>
        val (expr, extStore) = (Lib: LibInt).expr(name)(store)

        val enableRecursionEnv = expr match {
          case lr @ LamRef(_) =>
            Closure.addRecursiveClosure(name, lr, env)
          case _ => env
        }
        Async[F].pure(enableRecursionEnv -> extStore)
      case TopLevel.DataType(_) => Async[F].pure(env -> store)
      case b @ TopLevel.Binding(_) =>
        val (TopLevel.Binding(Declaration.Binding(name, _, expr)), extStore) =
          Declaration.TopLevel
            .compileBinding(b.asInstanceOf[TopLevel.Binding[Open.Expr]], store)

        val RequestReplyF(requestF, replyF) = asyncFuns(extStore)

        val enableRecursionEnv = expr match {
          case lr @ LamRef(_) => Closure.addRecursiveClosure(name, lr, env)
          case _              => env
        }

        implicit val s = extStore
        val valueF =
          runClient(expr, enableRecursionEnv)(requestF)(replyF)
        valueF.map { v =>
          env.add(name, v) -> extStore
        }
    }
  }

  // FIXME: Document this entire thing
  def runClient[F[_]: Async](term: Closed.Expr, env: Env)(
      requestF: CallInfo => F[Either[CallInfo, Value]])(
      replyF: Value => F[Either[CallInfo, Value]])(
      implicit store: LamStore): F[Value] = {
    def interpretRequestOrValue(
        reqOrVal: Either[ExternalCall, Value]): F[Value] = {
      val performExternalCall: ExternalCall => F[Value] = {
        case ExternalCall(callInfo, cont) =>
          def interpretResponseF(
              responseF: F[Either[CallInfo, Value]]): F[Value] = {
            val continueValue: Value => F[Value] =
              interpretRequestOrValue _ compose cont

            val continueCall: CallInfo => F[Value] = {
              case CallInfo(lr, bound, callEnv) => {
                store.get(lr) match {
                  case Some(
                      Closed
                        .ClosedLam(_, body, _, _, _, _, _, _)) =>
                    val result =
                      runClient(body, callEnv)(requestF)(replyF)
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
      cpsInterpretGeneral(term, env, Location.client)(x => Right(x))
    interpretRequestOrValue(valueOrExternalCall)
  }

  private[rpc] def cpsInterpretGeneral(
      term: Closed.Expr,
      env: Env,
      localLoc: Location.Loc)(cont: Cont[Value])(
      implicit store: LamStore): Either[ExternalCall, Value] = {

    def combinedCpsInterpret(results: List[Closed.Expr])(
        cont: Cont[List[Value]]): Either[ExternalCall, Value] = {
      def helper(values: List[Value], results: List[Closed.Expr])(
          cont: Cont[List[Value]]): Either[ExternalCall, Value] =
        results match {
          case Nil => cont(values.reverse)
          case e :: es =>
            cpsInterpretGeneral(e, env, localLoc) { v =>
              helper(v :: values, es)(cont)
            }
        }
      helper(Nil, results)(cont)
    }

    term match {
      case Closed.Native(name, vars) =>
        combinedCpsInterpret(vars) { vals =>
          cont((Lib: LibInt).fun(name)(vals))
        }
      case Closed.TypeApp(expr, List(tpe)) =>
        cpsInterpretGeneral(expr, env, localLoc) {
          case Closure(lr, closedEnv) =>
            val ClosedLam(_, body, _, List(tvar), _, _, _, _) = store(lr)

            val tEnv = closedEnv.add(tvar.str, tpe)
            cpsInterpretGeneral(body, tEnv, localLoc)(cont)
        }
      case Closed.LocApp(expr, List(locV)) =>
        cpsInterpretGeneral(expr, env, localLoc) {
          case Closure(lr, closedEnv) =>
            val ClosedLam(_, body, _, _, List(lvar), _, _, _) = store(lr)
            val loc = locV match {
              case l @ Location.Loc(_) => l
              case Location.Var(name)  => env.locs(name)
            }
            val lEnv = closedEnv.add(lvar.name, loc)
            cpsInterpretGeneral(body, lEnv, localLoc)(cont)
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
        cpsInterpretGeneral(expr, env, localLoc) {
          case v @ Value.Constructed(tag, args) =>
            alts.find(_.name == tag) match {
              case Some(Alternative(_, params, body)) =>
                val newEnv = params.zip(args).foldLeft(env) {
                  case (envAcc, (k, v)) =>
                    envAcc.add(k, v)
                }
                cpsInterpretGeneral(body, newEnv, localLoc)(cont)
              case None => throw CaseError(v, alts)
            }
        }

      case Closed.Let(bindings, expr) =>
        combinedCpsInterpret(bindings.map(_.expr)) { values =>
          val newEnv = bindings.zip(values).foldLeft(env) {
            case (accEnv, (b, value)) =>
              accEnv.add(b.name, value)
          }
          cpsInterpretGeneral(expr, newEnv, localLoc)(cont)
        }

      case Closed.Lit(literal) => cont(Constant(literal))
      case Closed.Var(name) =>
        env.value(name) match {
          case Some(v) => cont(v)
          case None =>
            throw MissingValueError(name, env.values)
        }

      // FIXME: Only close over the minimal needed environment
      case lr @ Closed.LamRef(_) => cont(Closure(lr, env))
      case Closed.App(fun, param, Some(locV)) =>
        cpsInterpretGeneral(fun, env, localLoc) {
          case Closure(lr, closedEnv) =>
            cpsInterpretGeneral(param, env, localLoc) { value => // evaluate param
              val Closed.ClosedLam(_, body, List(bound), free, _, _, _, _) =
                store(lr)

              val loc = locV match {
                case l @ Location.Loc(_) => l
                case Location.Var(name)  => env.locs(name)
              }
              if (loc == localLoc) {
                cpsInterpretGeneral(body,
                                    closedEnv.add(bound.name, value),
                                    localLoc)(cont)
              } else {
                Left(
                  ExternalCall(
                    CallInfo(
                      lr,
                      value,
                      closedEnv.add(bound.name, value)
//                      free.map { v =>
//                        // FIXME: go through all .get's and fix it with understandable errors
//                        closedEnv.value(v.name) match {
//                          case Some(v) => v
//                          case None =>
//                            throw MissingValueError(v.name, closedEnv.values)
//                        }
//                      }
                    ),
                    cont
                  ))
              }
            }
        }
    }
  }

  def performServerRequest(callInfo: CallInfo)(
      implicit store: LamStore): Either[ExternalCall, Value] = {
    val CallInfo(lr, bound, env) = callInfo
    store.get(lr) match {
      case Some(Closed.ClosedLam(_, body, _, _, _, _, _, _)) =>
        Interpreter.cpsInterpretGeneral(body, env, Location.server)(Right.apply)
      case None =>
        throw new RuntimeException(s"No $lr in store $store")
    }
  }

  def handleClientResponse[F[_]: Async](
      value: Value,
      queue: mutable.Queue[Cont[Value]]): Either[ExternalCall, Value] = {
    val cont = queue.dequeue()
    cont(value)
  }
}
