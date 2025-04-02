/*
 * Copyright 2025 capslock.dev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dev.capslock.mcpscala

import cats.effect.IO
import cats.effect.IOApp
import com.comcast.ip4s._
import org.http4s.ember.server._
import org.http4s.implicits._
import org.http4s.server.Router
import dev.capslock.mcpscala.web.Server
import dev.capslock.mcpscala.web.JsonRpc._
import dev.capslock.mcpscala.web.JsonRpc.Error
import io.circe._
import dev.capslock.mcpscala.mcp.*
import MethodIsJsonRpc.{*, given}

object Main extends IOApp.Simple {
  import io.circe.syntax.* // for asJson
  val methodHandlers: Server.MethodHandlers =
    Map(
      "initialize" -> {
        case Params.ByName(values) =>
          val capabilities =
            values("capabilities").as[ClientCapabilities].getOrElse(null)
          val clientInfo =
            values("clientInfo").as[Implementation].getOrElse(null)
          val protocolVersion =
            values("protocolVersion").as[String].getOrElse("")
          val request = Method.Initialize(
            capabilities,
            clientInfo,
            protocolVersion
          )
          // do initialize
          val response = InitializeResult(
            ServerCapabilities(
            ),
            Some("This server is still under development"),
            protocolVersion,
            Implementation("MCP Scala", "0.1.0")
          )
          IO.pure(
            Right(
              response.asJson
            )
          )
        case _ =>
          IO.pure(
            Left(
              Error(
                ErrorCode.InvalidParams,
                "Expected capabilities, clientInfo, and protocolVersion"
              )
            )
          )
      },
      "echo" -> { params =>
        IO.pure(
          params match {
            case Params.ByPosition(values) if values.nonEmpty =>
              Right(values.head)
            case Params.ByName(values) if values.nonEmpty =>
              Right(values.values.head)
            case _ =>
              Left(
                Error(
                  ErrorCode.InvalidParams,
                  "Expected at least one parameter"
                )
              )
          }
        )
      },
      "add" -> { params =>
        IO.pure(
          params match {
            case Params.ByPosition(values) if values.length >= 2 =>
              (values(0).asNumber, values(1).asNumber) match {
                case (Some(a), Some(b)) =>
                  Right(
                    Json.fromBigDecimal(
                      a.toBigDecimal.getOrElse(BigDecimal(0)) + b.toBigDecimal
                        .getOrElse(BigDecimal(0))
                    )
                  )
                case _ =>
                  Left(
                    Error(ErrorCode.InvalidParams, "Parameters must be numbers")
                  )
              }
            case Params.ByName(values)
                if values.contains("a") && values.contains("b") =>
              (values("a").asNumber, values("b").asNumber) match {
                case (Some(a), Some(b)) =>
                  Right(
                    Json.fromBigDecimal(
                      a.toBigDecimal.getOrElse(BigDecimal(0)) + b.toBigDecimal
                        .getOrElse(BigDecimal(0))
                    )
                  )
                case _ =>
                  Left(
                    Error(ErrorCode.InvalidParams, "Parameters must be numbers")
                  )
              }
            case _ =>
              Left(
                Error(
                  ErrorCode.InvalidParams,
                  "Expected two numeric parameters"
                )
              )
          }
        )
      }
    )

  val services = Server.jsonRpcService(methodHandlers)
  val httpApp = Router("/" -> services).orNotFound
  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(httpApp)
    .build
  def run: IO[Unit] =
    server.useForever
}
