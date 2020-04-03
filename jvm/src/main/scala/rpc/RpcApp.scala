package rpc

import java.io.File

import cats.effect.{Blocker, ExitCode, IO}
import cats.implicits._
import io.circe.parser._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s.{HttpRoutes, StaticFile}
import rpc.Declaration.TopLevel
import rpc.Expr.Open

trait RpcApp extends RpcAppInt {
  override def run(args: List[String]): IO[ExitCode] = {

    case class AppData(name: String,
                       raw: String,
                       decoded: List[TopLevel[Open.Expr]])
    def dataToRouterMapping(appData: AppData) = {
      val (_, store) = Interpreter.compileDeclarations(appData.decoded)
      s"/${appData.name}" -> ServerEvaluator.buildRoutes(appData.raw, store)
    }

    val programs = os.pwd / "src" / "main" / "resources" / "examples"
    val programTopLevelEithers =
      os.walk(programs).filter(_.ext == "json").map { path =>
        pprint.log(path.baseName)
        // FIXME: use PolyRpcCaller!
        val result = os.read(path)

        import HaskellJsonDecoders._
        val decoded = decode[List[TopLevel[Open.Expr]]](result)
        decoded.map { tls =>
          AppData(path.baseName.takeWhile(_ != '.'), result, tls)
        }
      }
    val eitherAppDatas = programTopLevelEithers.toList.sequence

    def assertMapping(blocker: Blocker) =
      "/" -> HttpRoutes
        .of[IO] {
          case req @ GET -> Root / "assets" / "client.js" =>
            // StaticFile.fromResource[IO]("test.html", blocker, Some(req)).getOrElseF(NotFound())
            StaticFile
              .fromFile(new File("../js/target/scala-2.13/rpc-fastopt.js"),
                        blocker,
                        Some(req))
              .getOrElseF(NotFound())
        }
    val eitherRun = eitherAppDatas.map { appDatas =>
      val routerMapping = appDatas.map(dataToRouterMapping)

      val resource = for {
        blocker <- Blocker[IO]
        server <- BlazeServerBuilder[IO]
          .bindHttp(port, hostname)
          .withHttpApp(
            Router(
              (assertMapping(blocker) :: routerMapping): _*
            ).orNotFound)
          .resource
      } yield server

      resource.use(_ => IO.never).as(ExitCode.Success)
    }

    eitherRun match {
      case Left(err) => IO { throw err }
      case Right(io) => io
    }
  }
}
