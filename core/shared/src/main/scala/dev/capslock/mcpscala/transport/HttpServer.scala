/*
 * Copyright (c) 2025 capslock.dev
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package dev.capslock.mcpscala.transport

import cats.effect.IO
import dev.capslock.mcpscala.Handler.MethodHandlers
import org.http4s.*
import org.http4s.dsl.io.*
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
