package dev.capslock.mcpscala.web

import cats.effect._, org.http4s._, org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`

object Server {
  def jsonRpcService = HttpRoutes.of[IO] {
    case GET -> Root =>
      Ok("MCP Scala.")
    case POST -> Root / "rpc" =>
      Ok(s"""{"message":"Hello."}""").map(
        _.withContentType(`Content-Type`(MediaType.application.json))
      )
  }
}
