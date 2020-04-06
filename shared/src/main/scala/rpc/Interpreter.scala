package rpc

import cats.effect.Async
import cats.implicits._
import io.circe.generic.JsonCodec
import rpc.Declaration.TopLevel
import rpc.Expr.Closed.{ClosedLam, LamRef, LamStore}
import rpc.Expr.{Closed, Open}
import rpc.Value._
import rpc.error.{CaseError, MissingValueError, TypeError}

import scala.collection.mutable

object Interpreter {
  @JsonCodec case class CallInfo(lamRef: LamRef, bound: Value, env: Env.Minimal)
  case class ExternalCall(callInfo: CallInfo, cont: Cont[Value])

  /**
    * Interpreter continuation of a type A to either an external call or a value.
    * @tparam A
    */
  type Cont[A] = A => Either[ExternalCall, Value]

  case class RequestReplyF[F[_]: Async](
      requestF: CallInfo => F[Either[CallInfo, Value]],
      replyF: Value => F[Either[CallInfo, Value]])

  def runDeclarations[F[_]: Async](decls: List[TopLevel[Open.Expr]],
                                   asyncFuns: RequestReplyF[F]): F[Env] = {
    val (closedDecls, store) = compileDeclarations(decls)
    closedDecls
      .foldLeft(Async[F].pure(Env.empty)) { (accIO, dec) =>
        accIO.flatMap { acc =>
          Interpreter.processDeclaration(dec, acc, store, asyncFuns)
        }
      }
  }

  def compileDeclarations(decls: List[TopLevel[Open.Expr]])
    : (List[TopLevel[Closed.Expr]], LamStore) =
    decls.foldLeft(List.empty[TopLevel[Closed.Expr]],
                   (Lib: LibInt).libraryStore) {
      case ((list, store), topLevel) =>
        val (expr, nStore) = compileDeclaration(topLevel, store)
        (list :+ expr, nStore)
    }

  private def compileDeclaration(
      decl: TopLevel[Open.Expr],
      store: LamStore): (TopLevel[Closed.Expr], LamStore) =
    decl match {
      case b @ TopLevel.Binding(_) =>
        Declaration.TopLevel
          .compileBinding(b.asInstanceOf[TopLevel.Binding[Open.Expr]], store)
      case TopLevel.DataType(d) => TopLevel.DataType[Closed.Expr](d) -> store
      case TopLevel.Library(n, t) =>
        TopLevel.Library[Closed.Expr](n, t) -> store
    }

  private def processDeclaration[F[_]: Async](
      decl: TopLevel[Closed.Expr],
      env: Env,
      store: LamStore,
      asyncFuns: RequestReplyF[F]): F[Env] = {
    decl match {
      case TopLevel.DataType(_) => Async[F].pure(env)
      // Tie the recursive not through a recursive environment on a closure
      case TopLevel.Library(name, _) =>
        val lamRef = (Lib: LibInt).expr(name)
        val enableRecursionEnv =
          Closure.addRecursiveClosure(name, lamRef, env, store)
        Async[F].pure(enableRecursionEnv)
      // Tie the recursive knot through a recursive environment on a closure
      case TopLevel.Binding(Declaration.Binding(name, _, expr)) =>
        val enableRecursionEnv = expr match {
          case lr @ LamRef(_) =>
            Closure.addRecursiveClosure(name, lr, env, store)
          case _ => env
        }

        implicit val s = store
        val valueF     = runClient(expr, enableRecursionEnv)(asyncFuns)
        valueF.map { v =>
          env.add(name, v)
        }
    }
  }

  def runClient[F[_]: Async](expr: Closed.Expr, env: Env)(
      requestReplyF: RequestReplyF[F])(implicit store: LamStore): F[Value] = {

    // interpret an external call or a value completely
    def interpretRequestOrValue(
        reqOrVal: Either[ExternalCall, Value]): F[Value] = {

      // interpret an external call fully
      val performExternalCall: ExternalCall => F[Value] = {
        case ExternalCall(callInfo, cont) =>
          // interpret a response matching an external call
          //   2 options: (1) a request from the other side
          //              (2) a value to continue the interpreter
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
                        requestReplyF)
                    val responseF =
                      Async[F].flatMap(result)(requestReplyF.replyF)
                    interpretResponseF(responseF)
                  case None =>
                    throw new RuntimeException(s"No $lr in store $store")
                }
              }
            }
            responseF.map(_.fold(continueCall, continueValue)).flatten
          }
          interpretResponseF(requestReplyF.requestF(callInfo))
      }

      // Flatten the interpreted external call case
      val result: Either[Value, F[Value]] =
        reqOrVal.swap.map(performExternalCall)
      result.sequence.map(_.merge)
    }

    // interpret the starting expression on the client location to retrieve either
    //   a value or a call to a different tier
    val valueOrExternalCall: Either[ExternalCall, Value] =
      interpretToValueOrExternalCall(expr, env, Location.client)(x => Right(x))

    // interpret the external call or continue interpreting the value
    interpretRequestOrValue(valueOrExternalCall)
  }

  private[rpc] def interpretToValueOrExternalCall(
      term: Closed.Expr,
      env: Env,
      localLoc: Location.Loc)(cont: Cont[Value])(
      implicit store: LamStore): Either[ExternalCall, Value] = {

    println(s"interpreting: $term")

    def interpretMultiple(results: List[Closed.Expr])(
        cont: Cont[List[Value]]): Either[ExternalCall, Value] = {
      def helper(values: List[Value], results: List[Closed.Expr])(
          cont: Cont[List[Value]]): Either[ExternalCall, Value] =
        results match {
          case Nil => cont(values.reverse)
          case e :: es =>
            interpretToValueOrExternalCall(e, env, localLoc) { v =>
              helper(v :: values, es)(cont)
            }
        }
      helper(Nil, results)(cont)
    }

    term match {
      case Closed.Native(name, vars) =>
        interpretMultiple(vars) { vals =>
          cont {
            pprint.log("Starting Lib")
            val x = (Lib: LibInt).fun(name)(vals)
            pprint.log("Ending Lib")
            x
          }
        }
      case Closed.TypeApp(expr, List(tpe)) =>
        interpretToValueOrExternalCall(expr, env, localLoc) {
          case Closure(lr, closedEnv) =>
            val ClosedLam(_, body, _, List(tvar), _, _, _, _) = store(lr)

            val tEnv = closedEnv.toEnv.add(tvar.str, tpe)
            interpretToValueOrExternalCall(body, tEnv, localLoc)(cont)
          case e => throw TypeError(e, "Type Abstraction")
        }
      case Closed.LocApp(expr, List(locV)) =>
        interpretToValueOrExternalCall(expr, env, localLoc) {
          case Closure(lr, closedEnv) =>
            val ClosedLam(_, body, _, _, List(lvar), _, _, _) = store(lr)
            val loc = locV match {
              case l @ Location.Loc(_) => l
              case Location.Var(name)  => env.locs(name)
            }
            val lEnv = closedEnv.toEnv.add(lvar.name, loc)
            interpretToValueOrExternalCall(body, lEnv, localLoc)(cont)
          case e => throw TypeError(e, "Location Abstraction")
        }
      case Closed.Tup(exprs) =>
        interpretMultiple(exprs) { values =>
          cont(Value.Tupled(values))
        }
      case Closed.Prim(op, args) =>
        interpretMultiple(args) { values =>
          val constants = values.map {
            case Value.Constant(lit) => lit
            case e                   => throw TypeError(e, "Constant")
          }
          cont(Value.Constant(Operator.operators(op)(constants)))
        }

      case Closed.Constructor(name, _, _, exprs) =>
        interpretMultiple(exprs) { values =>
          cont(Value.Constructed(name, values))
        }
      case Closed.Case(expr, alternatives) =>
        interpretToValueOrExternalCall(expr, env, localLoc) { v =>
          def performAlt(args: List[Value],
                         params: List[String],
                         body: Closed.Expr) = {
            val newEnv = params.zip(args).foldLeft(env) {
              case (envAcc, (k, v)) =>
                envAcc.add(k, v)
            }
            interpretToValueOrExternalCall(body, newEnv, localLoc)(cont)
          }

          v match {
            case Value.Constant(Literal.Bool(b)) =>
              val alts = alternatives.collect {
                case a: Alternative.Alt[Closed.Expr] => a
              }
              alts.find(_.name.toLowerCase == b.toString) match {
                case Some(Alternative.Alt(_, _, body)) =>
                  interpretToValueOrExternalCall(body, env, localLoc)(cont)
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
        interpretMultiple(bindings.map(_.expr)) { values =>
          val newEnv = bindings.zip(values).foldLeft(env) {
            case (accEnv, (b, value)) =>
              accEnv.add(b.name, value)
          }
          interpretToValueOrExternalCall(expr, newEnv, localLoc)(cont)
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
        interpretToValueOrExternalCall(fun, env, localLoc) {
          case Closure(lr, closedEnv) =>
            interpretToValueOrExternalCall(param, env, localLoc) { value =>
              val Closed.ClosedLam(_, body, List(bound), _, _, _, _, _) =
                store(lr)

              val loc = locV match {
                case l @ Location.Loc(_) => l
                case Location.Var(name)  => env.locs(name)
              }
              if (loc == localLoc) {
                interpretToValueOrExternalCall(
                  body,
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
        Interpreter.interpretToValueOrExternalCall(
          body,
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
