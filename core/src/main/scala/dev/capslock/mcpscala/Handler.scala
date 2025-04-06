package dev.capslock.mcpscala

import io.circe.syntax._
import cats.effect.IO
import dev.capslock.mcpscala.mcp.*
import dev.capslock.mcpscala.JsonRpc.Params
import scala.annotation.experimental

object Handler {
  type MethodHandler[F[_]] =
    JsonRpc.Params => F[Either[JsonRpc.Error, io.circe.Json]]
  type MethodHandlers[F[_]] = Map[String, MethodHandler[F]]

  private def handleInitialize(
      capabilities: ClientCapabilities,
      clientInfo: Implementation,
      protocolVersion: String
  ): IO[InitializeResult] = {
    val response = InitializeResult(
      ServerCapabilities(
        tools = Some(Tools()) // TODO: 実際のツール情報
      ),
      Some("This server is still under development"),
      protocolVersion,
      Implementation("MCP Scala", "0.1.0")
    )
    IO.pure(response)
  }

  private def handleListTools(
      cursor: Option[String]
  ): IO[ListToolsResult] = {
    // TODO: cursor を使ったページネーション
    import io.circe.Json
    val response = ListToolsResult(
      List(
        Tool(
          inputSchema = Json.obj(
            "type" -> Json.fromString("object"),
            "properties" -> Json.obj(
              "min" -> Json.obj("type" -> Json.fromString("number")),
              "max" -> Json.obj("type" -> Json.fromString("number"))
            ),
            "required" -> Json
              .arr(Json.fromString("min"), Json.fromString("max"))
          ),
          name = "randomNumber"
        )
      )
    )
    IO.pure(response)
  }

  private def handleCallTool(
      name: String,
      arguments: Option[Map[String, io.circe.Json]]
  ): IO[CallToolResult] = {
    name match {
      case "randomNumber" =>
        arguments match {
          case Some(args) =>
            val minResult = args.get("min").flatMap(_.as[Int].toOption)
            val maxResult = args.get("max").flatMap(_.as[Int].toOption)

            (minResult, maxResult) match {
              case (Some(min), Some(max)) =>
                if (min >= max) {
                  IO.pure(
                    CallToolResult(
                      isError = true,
                      content = Seq(
                        ContentPart.TextContentPart("min must be less than max")
                      )
                    )
                  )
                } else {
                  val random = scala.util.Random.between(min, max)
                  IO.pure(
                    CallToolResult(
                      isError = false,
                      content =
                        Seq(ContentPart.TextContentPart(random.toString))
                    )
                  )
                }
              case _ =>
                IO.pure(
                  CallToolResult(
                    isError = true,
                    content = Seq(
                      ContentPart.TextContentPart(
                        "min and max arguments are required and must be integers"
                      )
                    )
                  )
                )
            }
          case None =>
            IO.pure(
              CallToolResult(
                isError = true,
                content = Seq(
                  ContentPart.TextContentPart(
                    "arguments are required for randomNumber"
                  )
                )
              )
            )
        }
      case _ =>
        IO.pure(
          CallToolResult(
            isError = true,
            content =
              Seq(ContentPart.TextContentPart(s"Tool '$name' not found"))
          )
        )
    }
  }

  def byNameHandler[F[_]: cats.Applicative, In, Out](
      func: In => F[Either[String, Out]]
  )(using io.circe.Decoder[In], io.circe.Encoder[Out]): MethodHandler[F] = {
    (params: JsonRpc.Params) =>
      params match
        case Params.ByPosition(values) =>
          import cats.syntax.applicative.*
          Left(
            JsonRpc.Error(
              JsonRpc.ErrorCode.MethodNotFound,
              "Method is not defined for byPosition"
            )
          ).pure[F]

        case Params.ByName(values) =>
          values.asJson.as[In] match
            case Left(value) =>
              import cats.syntax.applicative.*
              Left(
                JsonRpc.Error(
                  JsonRpc.ErrorCode.InvalidParams,
                  "Invalid parameters",
                  None
                )
              ).pure[F]
            case Right(p) =>
              import cats.syntax.functor.*
              func(p) map {
                case Left(error) =>
                  Left(
                    JsonRpc.Error(
                      JsonRpc.ErrorCode.InvalidParams,
                      error,
                      None
                    )
                  )
                case Right(result) =>
                  Right(result.asJson)
              }
  }

  @experimental
  def byPositionHandler[F[_]: cats.Applicative, In <: Tuple, Out](
      func: In => F[Either[String, Out]]
  )(using
      io.circe.Decoder[In],
      io.circe.Encoder[Out]
  ): MethodHandler[F] = { (params: JsonRpc.Params) =>
    params match
      case Params.ByPosition(values) =>
        values.asJson.as[In] match
          case Left(value) =>
            import cats.syntax.applicative.*
            Left(
              JsonRpc.Error(
                JsonRpc.ErrorCode.InvalidParams,
                "Invalid parameters",
                None
              )
            ).pure[F]
          case Right(p) =>
            // TODO: extract p and pass
            import cats.syntax.functor.*
            func(p) map {
              case Left(error) =>
                Left(
                  JsonRpc.Error(
                    JsonRpc.ErrorCode.InvalidParams,
                    error,
                    None
                  )
                )
              case Right(result) =>
                Right(result.asJson)
            }
      case Params.ByName(values) =>
        import cats.syntax.applicative.*
        Left(
          JsonRpc.Error(
            JsonRpc.ErrorCode.MethodNotFound,
            "Method is not defined for byName"
          )
        ).pure[F]
  }

  import MethodIsJsonRpc.given
  val methodHandlers: MethodHandlers[IO] = Map(
    "initialize" -> byNameHandler { (params: Method.Initialize) =>
      {
        handleInitialize(
          params.capabilities,
          params.clientInfo,
          params.protocolVersion
        ).map(x => Right(x))
      }
    },
    "tools/list" -> { (params: JsonRpc.Params) =>
      params match
        case Params.ByPosition(values) =>
          values.size match
            case 0 =>
              for result <- handleListTools(None)
              yield Right(result.asJson)
            case _ =>
              values.head.as[Method.ListTools] match
                case Left(value) =>
                  IO.pure(
                    Left(
                      JsonRpc.Error(
                        JsonRpc.ErrorCode.InvalidParams,
                        "Invalid parameters for 'tools/list'",
                        None
                      )
                    )
                  )
                case Right(p) =>
                  for result <- handleListTools(
                      p.cursor
                    )
                  yield Right(result.asJson)

        case Params.ByName(values) =>
          IO.pure(
            Left(
              JsonRpc.Error(
                JsonRpc.ErrorCode.MethodNotFound,
                "Method 'tools/list' is not defined for byPosition"
              )
            )
          )

    },
    "tools/call" -> {
      byNameHandler { (params: Method.CallTool) =>
        handleCallTool(params.name, params.arguments).map(x => Right(x))
      }
    }
  )
}
