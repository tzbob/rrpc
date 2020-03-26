package rpc

import cats.effect.Async
import cats.implicits._
import io.circe.generic.JsonCodec
import rpc.Declaration.TopLevel
import rpc.Expr.Closed.{ClosedLam, LamRef, LamStore}
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

  def runDeclarations[F[_]: Async](decls: List[TopLevel])(
      asyncFuns: LamStore => RequestReplyF[F]): F[Env] = {
    decls.foldLeft(Async[F].pure(Env.empty)) { (accIO, dec) =>
      accIO.flatMap { acc =>
        Interpreter.processDeclaration(dec, acc)(asyncFuns)
      }
    }
  }

  def processDeclaration[F[_]: Async](decl: TopLevel, env: Env)(
      asyncFuns: LamStore => RequestReplyF[F]): F[Env] = {
    decl match {
      case TopLevel.Library(name, _) =>
        val (expr, store) = (Lib: LibInt).expr(name)(env.lamStore)
        Async[F].pure(env.add(name, expr).mergeStore(store))
      case TopLevel.DataType(_) => Async[F].pure(env)
      case b @ TopLevel.Binding(_) =>
        val (TopLevel.Binding(Declaration.Binding(name, _, expr)), store) =
          Declaration.TopLevel.compileBinding(
            b.asInstanceOf[TopLevel.Binding[Open.Expr]],
            env.lamStore)

        val RequestReplyF(requestF, replyF) = asyncFuns(store)
        val valueF =
          runClient(expr, env.add(name, expr).mergeStore(store))(requestF)(
            replyF)
        valueF.map { v =>
          env.add(name, v).mergeStore(store)
        }
    }
  }

  // FIXME: Document this entire thing
  def runClient[F[_]: Async](term: Closed.Expr, env: Env)(
      requestF: CallInfo => F[Either[CallInfo, Value]])(
      replyF: Value => F[Either[CallInfo, Value]]): F[Value] = {
    val store = env.lamStore
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
                  case Some(
                      Closed
                        .ClosedLam(_, body, List(boundVar), freeVars, _, _)) =>
                    val newValueEnv = freeVars
                      .map(_.name)
                      .zip(frees)
                      .toMap + (boundVar.name -> bound)
                    val newEnv = env.copy(values = newValueEnv)
                    val result =
                      runClient(body, newEnv)(requestF)(replyF)
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

  private[rpc] def cpsInterpretGeneral(term: Closed.Expr,
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
            cpsInterpretGeneral(e, env, localLoc) { v =>
              helper(v :: values, es)(cont)
            }
        }
      helper(Nil, results)(cont)
    }

    def extendEnv[A](lookupEnv: Env, keys: List[String], extendedEnv: Env)(
        reader: (Env, String) => Option[A])(writer: (Env, String, A) => Env) = {
      keys
        .map(k => reader(lookupEnv, k).map(v => k -> v))
        .flatten
        .foldLeft(extendedEnv) {
          case (envAcc, (k, v)) => writer(envAcc, k, v)
        }
    }

    val store = env.lamStore

    pprint.log(env.values)
    pprint.log(term)

    term match {
      case Closed.Native(name, vars) =>
        combinedCpsInterpret(vars) { vals =>
          cont((Lib: LibInt).fun(name)(vals))
        }
      case Closed.TypeApp(expr, tpes) =>
        cpsInterpretGeneral(expr, env, localLoc) {
          case Closure(lr, closedEnv) =>
            val ClosedLam(_, body, _, _, tvars, _) = store(lr)
            val tEnv = tvars.zip(tpes).foldLeft(closedEnv) {
              case (acc, (name, tpe)) =>
                acc.add(name.str, tpe)
            }
            cpsInterpretGeneral(body, tEnv, localLoc)(cont)
        }
      case Closed.LocApp(expr, locs) =>
        cpsInterpretGeneral(expr, env, localLoc) {
          case Closure(lr, closedEnv) =>
            val ClosedLam(_, body, _, _, _, lVars) = store(lr)
            val lEnv = lVars.zip(locs).foldLeft(closedEnv) {
              case (acc, (name, loc)) =>
                acc.add(name.name, loc)
            }
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
          case None =>
            cpsInterpretGeneral(env.recursive(name), env, localLoc)(cont)
          case Some(v) => cont(v)
        }

      // FIXME: Only close over the minimal needed environment
      case lr @ Closed.LamRef(_) => cont(Closure(lr, env))
      case a @ Closed.App(fun, param, Some(locV)) =>
        val loc = locV match {
          case l @ Location.Loc(_) => l
          case Location.Var(name)  => env.locs(name)
        }

        println(s"start interpret of fun $fun")
        cpsInterpretGeneral(fun, env, localLoc) {
          case Closure(lr, closedEnv) =>
            println(s"start interpret of fun $param")
            cpsInterpretGeneral(param, env, localLoc) { value => // evaluate param
              // FIXME: problem here is that bound is needed to fill up the environment.
              //  Instead bound (or the holes in the environments could be added to lamref?
              val Closed.ClosedLam(_, body, List(bound), free, _, _) =
                store(lr)
              if (loc == localLoc) {
                cpsInterpretGeneral(body,
                                    closedEnv.add(bound.name, value),
                                    localLoc)(cont)
              } else {
                Left(ExternalCall(CallInfo(lr, value, free.map { v =>
                  closedEnv.value(v.name).get
                }), cont))
              }
            }
        }
    }
  }

  def performServerRequest(store: LamStore,
                           callInfo: CallInfo): Either[ExternalCall, Value] = {
    val CallInfo(lr, bound, frees) = callInfo
    store.get(lr) match {
      case Some(Closed.ClosedLam(_, body, List(boundVar), freeVars, _, _)) =>
        val newVEnv = freeVars
          .map(_.name)
          .zip(frees)
          .toMap + (boundVar.name -> bound)
        Interpreter.cpsInterpretGeneral(
          body,
          Env.empty.copy(values = newVEnv).mergeStore(store),
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
