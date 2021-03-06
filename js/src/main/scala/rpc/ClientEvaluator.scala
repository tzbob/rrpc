package rpc

import java.util.UUID

import cats.effect.{ContextShift, IO}
import io.circe.parser._
import io.circe.{Decoder, Encoder}
import izumi.logstage.api.IzLogger
import org.scalajs.dom
import org.scalajs.dom.{Event, KeyboardEvent}
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.HTMLInputElement
import rpc.Declaration.TopLevel
import rpc.Expr.Closed.LamStore
import rpc.Expr.{Closed, Open}
import rpc.Interpreter.{CallInfo, RequestReplyF}
import rpc.Mvu.Binding
import snabbdom.VNode

object ClientEvaluator {
  private val logger     = Log.logger
  private val identifier = UUID.randomUUID()

  private def caller[A: Encoder](uri: String)(a: A)(
      implicit ctx: ContextShift[IO]): IO[Either[CallInfo, Value]] = {

    implicit val idEnc = implicitly[Encoder[Identified[A]]]
    implicit val decoder =
      implicitly[Decoder[CallInfo]] either implicitly[Decoder[Value]]

    val responseIO = IO.fromFuture(IO {
      Ajax
        .post(uri, idEnc.apply(Identified(identifier, a)).noSpaces)
    })

    responseIO.map { xhr =>
      val decoded = decode[Either[CallInfo, Value]](xhr.responseText)
      decoded match {
        case Left(err)  => throw err
        case Right(res) => res
      }
    }
  }

  def buildClientRun(
      declarations: List[TopLevel[Open.Expr]],
      uri: String,
      appName: String)(implicit ctx: ContextShift[IO]): IO[Env] = {
    Interpreter
      .compileAndRunDeclarations(declarations, mkRequestReplyF(uri, appName))
  }

  def buildHtmlRun(
      declarations: List[TopLevel[Open.Expr]],
      uri: String,
      appName: String)(implicit ctx: ContextShift[IO]): IO[Unit] = {
    val requestReplyF = mkRequestReplyF(uri, appName)

    val (closedDecls, store) = Interpreter.compileDeclarations(declarations)
    val envIO =
      Interpreter.runDeclarations(requestReplyF, closedDecls, store, Env.empty)

    envIO.map { env =>
      env.value("main") match {
        case Some(v) =>
          def vNodeFromValue(v: Value): VNode = {
            Mvu.htmlFromValue(v) { (ev, binding) =>
              // In the RecHtml runner this computes is the new Html page
              val newHtml =
                computeEventBinding(requestReplyF, store, env, ev, binding)
              val vNodeOptIO = newHtml.map(_.map(vNodeFromValue))
              val patcherIO = vNodeOptIO.map {
                case None =>
                  throw new RuntimeException(
                    s"Could not compute Html for $ev with $binding")
                case Some(vnode) =>
                  domPatcher.applyNewState(vnode)
              }
              // execute the handler to patch the new html
              patcherIO.unsafeRunAsyncAndForget()
            }
          }

          lazy val domPatcher =
            new DomPatcher(vNodeFromValue(v),
                           dom.document.getElementById("body"))
          domPatcher
        case None =>
          throw new RuntimeException(s"No value for main: $env")
      }
    }
  }

  def buildPageRun(
      declarations: List[TopLevel[Open.Expr]],
      uri: String,
      appName: String)(implicit ctx: ContextShift[IO]): IO[DomPatcher] = {
    import Mvu._

    val requestReplyF = mkRequestReplyF(uri, appName)

    val (closedDecls, store) = Interpreter.compileDeclarations(declarations)
    val envIO =
      Interpreter.runDeclarations(requestReplyF, closedDecls, store, Env.empty)

    envIO.flatMap { env =>
      val pageOpt = env.value("main").flatMap(Mvu.pageFromValue)

      pageOpt match {
        case None =>
          throw new RuntimeException(s"${env.value("main")} is not a Page")
        case Some(page) =>
          var currentModel = page.init

          logger.trace(s"(main): $page")
          val initialHtmlIO =
            applyValueToClosure(requestReplyF,
                                store,
                                page.view,
                                List(page.init),
                                env)

          initialHtmlIO.map { initViewOpt =>
            def vNodeFromValueOpt(initViewOpt: Option[Value],
                                  patcher: => DomPatcher): VNode =
              initViewOpt match {
                case None => throw new RuntimeException("No initial view value")
                case Some(value) =>
                  Mvu.htmlFromValue(value) { (evt, binding) =>
                    logger.info(s"Event handler triggered: $binding")

                    val optValueIO = computeEventBinding(requestReplyF,
                                                         store,
                                                         env,
                                                         evt,
                                                         binding)

                    val optModelIO = optValueIO.flatMap {
                      case Some(msgValue) =>
                        logger.info(s"new msg $msgValue")
                        applyValueToClosure(requestReplyF,
                                            store,
                                            page.update,
                                            List(msgValue, currentModel),
                                            env)
                      case None => IO.pure(None)
                    }

                    val handlerIO = optModelIO.flatMap {
                      case Some(modelValue) =>
                        logger.info(s"new model $modelValue")
                        currentModel = modelValue // current Model has been updated
                        val optHtmlValueIO =
                          applyValueToClosure(requestReplyF,
                                              store,
                                              page.view,
                                              List(modelValue),
                                              env)
                        val vnodeIO =
                          optHtmlValueIO.map(vNodeFromValueOpt(_, patcher))
                        vnodeIO.map(patcher.applyNewState)
                      case None => IO.pure(None)
                    }
                    handlerIO
                      .unsafeRunAsyncAndForget() // execute IO in the handler
                  }

              }

            object InitPatcher {
              val vnode: VNode = vNodeFromValueOpt(initViewOpt, patcher)
              logger.info(s"${vnode.children}")
              lazy val patcher =
                new DomPatcher(vnode,
                               dom.document.querySelector(page.mountPoint))
            }
            InitPatcher.patcher
          }
      }
    }
  }

  private def computeEventBinding(requestReplyF: RequestReplyF[IO],
                                  store: LamStore,
                                  env: Env,
                                  evt: Event,
                                  binding: Binding) = {
    binding match {
      case Binding.TargetValue(closure) =>
        applyValueToClosure(requestReplyF,
                            store,
                            closure,
                            List(
                              Value.Constant(
                                Literal.String(evt.target
                                  .asInstanceOf[HTMLInputElement]
                                  .value))),
                            env)
      case Binding.KeyPress(key, msg) =>
        if (evt
              .asInstanceOf[KeyboardEvent]
              .keyCode == key) IO.pure(Some(msg))
        else IO.pure(None)
      case Binding.EmptyValue(msg) =>
        IO.pure(Some(msg))
    }
  }

  private def mkRequestReplyF(uri: String, appName: String)(
      implicit ctx: ContextShift[IO]) = {
    RequestReplyF(caller[CallInfo](s"$uri/$appName/interpret"),
                  caller[Value](s"$uri/$appName/continue"))
  }

  private def applyValueToClosure(requestReplyF: RequestReplyF[IO],
                                  store: LamStore,
                                  closure: Value,
                                  params: List[Value],
                                  env: Env): IO[Option[Value]] = {
    val funName    = "$_tmp_fun"
    val paramName  = "$_tmp_param"
    val resultName = "$_tmp_result"

    val envTotal = params.zipWithIndex.foldLeft(env.add(funName, closure)) {
      case (acc, (param, idx)) =>
        acc.add(s"$paramName.$idx", param)
    }

    val app =
      (0 to params.size - 1).foldLeft(Closed.Var(funName): Closed.Expr) {
        (expr, idx) =>
          Closed.App(expr,
                     Closed.Var(s"$paramName.$idx"),
                     Some(Location.client))
      }

    logger.info(s"$app")

    Interpreter
      .runDeclarations(
        requestReplyF,
        List(
          TopLevel.Binding(
            Declaration.Binding[Closed.Expr](resultName, Tpe.Var("_"), app))),
        store,
        envTotal
      )
      .map(_.value(resultName))
  }
}
