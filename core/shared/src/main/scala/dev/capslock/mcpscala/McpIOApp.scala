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

package dev.capslock.mcpscala

import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.*
import com.monovore.decline.*
import com.monovore.decline.effect.*
import dev.capslock.mcpscala.Handler.MethodHandlers
import dev.capslock.mcpscala.transport.HttpServer
import dev.capslock.mcpscala.transport.StdioServer
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router

abstract class McpIOApp(
    name: String,
    header: String,
    version: Option[String] = None
) extends CommandIOApp(
      name = name,
      header = header,
      version = version.getOrElse("")
    ):

  def handlers: MethodHandlers[IO]

  val serveStdio =
    Opts.subcommand("stdio", help = "Serve MCP server over stdio transport"):
      Opts.unit.map(_ => StdioServer.serve(handlers))

  val serveHttp =
    Opts.subcommand("http", help = "Serve MCP server over HTTP transport"):
      val hostOpts = Opts
        .option[String]("host", help = "Host to bind the server")
        .withDefault("0.0.0.0")
        .map(Host.fromString)
      val portOpts = Opts
        .option[Int]("port", help = "Port to bind the server")
        .withDefault(8080)
      (hostOpts, portOpts).mapN: (host, port) =>
        val services = HttpServer.jsonRpcService(handlers)
        val httpApp = Router("/" -> services).orNotFound
        val server = EmberServerBuilder
          .default[IO]
          .withHost(host.getOrElse(ipv4"0.0.0.0"))
          .withPort(port"8080")
          .withHttpApp(httpApp)
          .build
        server.useForever

  override def main: Opts[IO[ExitCode]] =
    serveStdio.orElse(serveHttp).map(_.as(ExitCode.Success))
