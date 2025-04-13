package dev.capslock.mcpscala

import com.monovore.decline.*
import com.monovore.decline.effect.*
import dev.capslock.mcpscala.Handler.MethodHandlers
import cats.effect.IO
import dev.capslock.mcpscala.transport.StdioServer
import dev.capslock.mcpscala.transport.HttpServer
import cats.effect.ExitCode
import org.http4s.server.Router
import org.http4s.ember.server.EmberServerBuilder
import cats.syntax.all.*
import com.comcast.ip4s.*

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