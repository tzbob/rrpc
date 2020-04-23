package rpc

import cats.effect.IOApp

trait RpcAppInt extends IOApp {
  val hostname: String
  val port: Int
  val apps: Map[String, Either[String, (String, String)]]
}
