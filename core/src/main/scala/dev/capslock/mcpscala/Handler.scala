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

  def methodHandlers(tools: Map[String, server.Tool[?]]): MethodHandlers[IO] = {
    import MethodIsJsonRpc.given

    Map(
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
                for result <- handleListTools(tools)(None)
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
                    for result <- handleListTools(tools)(
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
          handleCallTool(tools)(params.name, params.arguments).map(x =>
            Right(x)
          )
        }
      }
    )
  }

  // private

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

  private def handleListTools(tools: Map[String, server.Tool[?]])(
      cursor: Option[String]
  ): IO[ListToolsResult] = {
    // TODO: pagenation
    val toolMap: Seq[Tool] = tools.map { case (name, tool) =>
      Tool(
        inputSchema = tool.inputSchema,
        name = name,
        description = tool.description
      )
    }.toSeq

    IO.pure(ListToolsResult(toolMap))
  }

  private def handleCallTool(tools: Map[String, server.Tool[?]])(
      name: String,
      arguments: Option[Map[String, io.circe.Json]]
  ): IO[CallToolResult] = {
    val tool = tools.get(name)
    tool match
      case None =>
        IO.pure(
          CallToolResult(
            isError = true,
            content = Seq(
              ContentPart.TextContentPart(s"Tool '$name' not found")
            )
          )
        )
      case Some(t) => {
        given io.circe.Decoder[t.Input] = t.inputDecoder
        val parsedInput = arguments.asJson.as[t.Input]
        parsedInput match
          case Left(value) =>
            IO.pure(
              CallToolResult(
                isError = true,
                content = Seq(
                  ContentPart.TextContentPart(
                    s"Parameter parse failed: Invalid parameters for tool '$name': ${value.message}"
                  )
                )
              )
            )
          case Right(input) =>
            t.func(input).map { result =>
              CallToolResult(isError = false, content = result)
            }
      }
  }

  private def byNameHandler[F[_]: cats.Applicative, In, Out](
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
  private def byPositionHandler[F[_]: cats.Applicative, In <: Tuple, Out](
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
}
