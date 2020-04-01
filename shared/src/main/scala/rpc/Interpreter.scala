package rpc

import cats.effect.Async
import cats.implicits._
import rpc.Declaration.TopLevel
import rpc.Expr.Closed.{ClosedLam, LamRef, LamStore}
import rpc.Expr.{Closed, Open}
import rpc.Value._
import rpc.error.{CaseError, MissingValueError, TypeError}

import scala.collection.mutable

object Interpreter {
  type TierApp[F[_]] = (LamStore,
                        Map[Closed.Var, Closed.Expr],
                        Location,
                        LamRef,
                        Closed.Expr) => F[Closed.Expr]

  case class CallInfo(lamRef: LamRef, bound: Value, env: Env.Minimal)
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
            Closure.addRecursiveClosure(name, lr, env, extStore)
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
          case lr @ LamRef(_) =>
            Closure.addRecursiveClosure(name, lr, env, extStore)
          case _ => env
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
              case CallInfo(lr, bound, minCallEnv) => {
                val callEnv = minCallEnv.toEnv
                store.get(lr) match {
                  case Some(
                      Closed
                        .ClosedLam(_, body, List(boundVar), _, _, _, _, _)) =>
                    val result =
                      runClient(body, callEnv.add(boundVar.name, bound))(
                        requestF)(replyF)
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

            val tEnv = closedEnv.toEnv.add(tvar.str, tpe)
            cpsInterpretGeneral(body, tEnv, localLoc)(cont)
          case e => throw TypeError(e, "Type Abstraction")
        }
      case Closed.LocApp(expr, List(locV)) =>
        cpsInterpretGeneral(expr, env, localLoc) {
          case Closure(lr, closedEnv) =>
            val ClosedLam(_, body, _, _, List(lvar), _, _, _) = store(lr)
            val loc = locV match {
              case l @ Location.Loc(_) => l
              case Location.Var(name)  => env.locs(name)
            }
            val lEnv = closedEnv.toEnv.add(lvar.name, loc)
            cpsInterpretGeneral(body, lEnv, localLoc)(cont)
          case e => throw TypeError(e, "Location Abstraction")
        }
      case Closed.Tup(exprs) =>
        combinedCpsInterpret(exprs) { values =>
          cont(Value.Tupled(values))
        }
      case Closed.Prim(op, args) =>
        combinedCpsInterpret(args) { values =>
          val constants = values.map {
            case Value.Constant(lit) => lit
            case e                   => throw TypeError(e, "Constant")
          }
          cont(Value.Constant(Operator.operators(op)(constants)))
        }

      case Closed.Constructor(name, _, _, exprs) =>
        combinedCpsInterpret(exprs) { values =>
          cont(Value.Constructed(name, values))
        }
      case Closed.Case(expr, alternatives) =>
        cpsInterpretGeneral(expr, env, localLoc) { v =>
          def performAlt(args: List[Value],
                         params: List[String],
                         body: Closed.Expr) = {
            val newEnv = params.zip(args).foldLeft(env) {
              case (envAcc, (k, v)) =>
                envAcc.add(k, v)
            }
            cpsInterpretGeneral(body, newEnv, localLoc)(cont)
          }

          v match {
            case Value.Constant(Literal.Bool(b)) =>
              val alts = alternatives.collect {
                case a: Alternative.Alt[Closed.Expr] => a
              }
              alts.find(_.name.toLowerCase == b.toString) match {
                case Some(Alternative.Alt(_, _, body)) =>
                  cpsInterpretGeneral(body, env, localLoc)(cont)
                case None => throw CaseError(v, alts)
              }
            case Value.Tupled(args) =>
              val alt = alternatives.collectFirst {
                case t: Alternative.TupAlt[Closed.Expr] => t
              }
              alt match {
                case Some(Alternative.TupAlt(params, body)) =>
                  performAlt(args, params, body)
                case None => throw CaseError(v, alt.toList)
              }
            case Value.Constructed(tag, args) =>
              val alts: List[Alternative.Alt[Closed.Expr]] = alternatives
                .collect { case a: Alternative.Alt[Closed.Expr] => a }
              alts.find(_.name == tag) match {
                case Some(Alternative.Alt(_, params, body)) =>
                  performAlt(args, params, body)
                case None => throw CaseError(v, alts)
              }
            case e => throw TypeError(e, "Tuple, DataType or Bool")
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

      case lr @ Closed.LamRef(_) =>
        val ClosedLam(_, _, _, _, _, freeVars, freeTpes, freeLocs) = store(lr)

        val minimal = Env.minimize(env, freeTpes, freeLocs, freeVars)
        cont(Closure(lr, minimal))
      case Closed.App(fun, param, Some(locV)) =>
        cpsInterpretGeneral(fun, env, localLoc) {
          case Closure(lr, closedEnv) =>
            cpsInterpretGeneral(param, env, localLoc) { value => // evaluate param
              val Closed.ClosedLam(_, body, List(bound), _, _, _, _, _) =
                store(lr)

              val loc = locV match {
                case l @ Location.Loc(_) => l
                case Location.Var(name)  => env.locs(name)
              }
              if (loc == localLoc) {
                cpsInterpretGeneral(body,
                                    closedEnv.toEnv.add(bound.name, value),
                                    localLoc)(cont)
              } else {
                Left(
                  ExternalCall(
                    CallInfo(lr, value, closedEnv.add(bound.name, value)),
                    cont
                  ))
              }
            }
          case e => throw TypeError(e, "Function")
        }
    }
  }

  def performServerRequest(callInfo: CallInfo)(
      implicit store: LamStore): Either[ExternalCall, Value] = {
    val CallInfo(lr, bound, env) = callInfo
    store.get(lr) match {
      case Some(Closed.ClosedLam(_, body, List(boundVar), _, _, _, _, _)) =>
        Interpreter.cpsInterpretGeneral(body,
                                        env.toEnv.add(boundVar.name, bound),
                                        Location.server)(Right.apply)
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
