package dev.capslock.mcpscala

import io.circe.syntax._ // for asJson
import cats.effect.IO
import JsonRpc._
import JsonRpc.Error
import io.circe._
import dev.capslock.mcpscala.mcp.*
import MethodIsJsonRpc.given

object Handler {
  type MethodHandler = Params => IO[Either[Error, Json]]
  type MethodHandlers = Map[String, MethodHandler]

  private def errorResponse(
      error: JsonRpc.Error
  ): IO[Either[JsonRpc.Error, Json]] =
    IO.pure(Left(error))

  private def invalidParams(message: String): IO[Either[JsonRpc.Error, Json]] =
    errorResponse(Error(ErrorCode.InvalidParams, message))

  private def methodNotFound(message: String): IO[Either[JsonRpc.Error, Json]] =
    errorResponse(Error(ErrorCode.MethodNotFound, message))

  val methodHandlers: MethodHandlers =
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
              tools = Some(Tools())
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
          invalidParams(
            "Expected capabilities, clientInfo, and protocolVersion"
          )
      },
      "tools/list" -> { params =>
        val response = ListToolsResult(
          List(
            Tool(
              inputSchema = Json.obj(
                "properties" -> Json.obj(
                  "min" -> Json.obj("type" -> Json.fromString("number")),
                  "max" -> Json.obj("type" -> Json.fromString("number"))
                ),
                "type" -> Json.fromString("object"),
                "required" -> Json
                  .arr(Json.fromString("min"), Json.fromString("max"))
              ),
              name = "randomNumber"
            )
          )
        )
        params match {
          case Params.ByPosition(values) =>
            val cursor = values.lift(0).flatMap(_.asString)
            // ignore cursor now
            IO.pure(Right(response.asJson))
          case Params.ByName(values) =>
            val cursor = values.get("cursor").flatMap(_.asString)
            // ignore cursor now
            IO.pure(Right(response.asJson))
        }
      },
      "tools/call" -> { params =>
        params match {
          case Params.ByName(values) =>
            val name = values.get("name").map(_.asString).flatten
            val arguments = values.get("arguments").map(_.asObject).flatten
            (name, arguments) match {
              case (Some(name), Some(arguments)) =>
                name match {
                  case "randomNumber" =>
                    val min =
                      arguments("min").flatMap(_.asNumber).flatMap(_.toInt)
                    val max =
                      arguments("max").flatMap(_.asNumber).flatMap(_.toInt)
                    (min, max) match {
                      case (Some(min), Some(max)) =>
                        val random = scala.util.Random.between(min, max)
                        IO.pure(
                          Right(
                            Json.obj(
                              "isError" -> Json.fromBoolean(false),
                              "content" -> Json.arr(
                                Json.obj(
                                  "type" -> Json.fromString("text"),
                                  "text" -> Json.fromString(random.toString())
                                )
                              )
                            )
                          )
                        )
                      case _ =>
                        invalidParams("min and max must be integers")
                    }
                  case _ =>
                    methodNotFound(s"Method $name not found")
                }
              case _ =>
                invalidParams("Expected name and arguments")
            }
        }
      }
    )
}
