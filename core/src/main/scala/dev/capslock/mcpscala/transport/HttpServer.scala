package dev.capslock.mcpscala.transport

import dev.capslock.mcpscala.Handler.MethodHandlers
import cats.effect.IO
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`

object HttpServer {
  def jsonRpcService(methodHandlers: MethodHandlers[IO] = Map.empty) =
    HttpRoutes.of[IO] {
      case GET -> Root =>
        Ok("MCP Scala.")
      case req @ POST -> Root / "rpc" =>
        req.as[String].flatMap { body =>
          IO.println(body) >> Server
            .handleJsonRpcRequest(methodHandlers)(body)
            .flatMap { response =>
              Ok(response)
                .map(
                  _.withContentType(`Content-Type`(MediaType.application.json))
                )
            }
        }
    }
}
