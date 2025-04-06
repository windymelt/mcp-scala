package dev.capslock.mcpscala

import io.circe.syntax._
import cats.effect.IO
import dev.capslock.mcpscala.JsonRpc // JsonRpc.scala が同じパッケージにあると仮定
import dev.capslock.mcpscala.mcp.*
import dev.capslock.mcpscala.macros.HandlerMacros.{
  byNameHandler
  // byPositionHandler,
} // 新しいマクロをインポート
import dev.capslock.mcpscala.JsonRpc.Params

object Handler {
  // MethodHandler と MethodHandlers の型エイリアスはマクロ利用側では不要になるかも
  type MethodHandler =
    JsonRpc.Params => IO[Either[JsonRpc.Error, io.circe.Json]]
  type MethodHandlers = Map[String, MethodHandler]

  trait MethodHandlerContext {
    type Params
    type Result
    def wrapper(x: JsonRpc.Params): IO[Either[String, Result]]
    def apply(x: Params): IO[Either[String, Result]]
  }
  type MethodHandlerContexts = Map[String, MethodHandlerContext]

  // --- ビジネスロジック関数 ---

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

  // Circe コーデック (Methods.scala に移動推奨)
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
                      content =
                        Seq(TextContentPart("min must be less than max"))
                    )
                  )
                } else {
                  val random = scala.util.Random.between(min, max)
                  IO.pure(
                    CallToolResult(
                      isError = false,
                      content = Seq(TextContentPart(random.toString))
                    )
                  )
                }
              case _ =>
                IO.pure(
                  CallToolResult(
                    isError = true,
                    content = Seq(
                      TextContentPart(
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
                  TextContentPart("arguments are required for randomNumber")
                )
              )
            )
        }
      case _ =>
        IO.pure(
          CallToolResult(
            isError = true,
            content = Seq(TextContentPart(s"Tool '$name' not found"))
          )
        )
    }
  }

  // --- マクロを使って methodHandlers を生成 ---
  import MethodIsJsonRpc.given
  val methodHandlers: MethodHandlers = Map(
    "initialize" -> { (params: JsonRpc.Params) =>
      params match
        case Params.ByPosition(values) =>
          // initialize is not defined
          IO.pure(
            Left(
              JsonRpc.Error(
                JsonRpc.ErrorCode.MethodNotFound,
                "Method 'initialize' is not defined for byPosition"
              )
            )
          )
        case Params.ByName(values) =>
          values.asJson.as[Method.Initialize] match
            case Left(value) =>
              IO.pure(
                Left(
                  JsonRpc.Error(
                    JsonRpc.ErrorCode.InvalidParams,
                    "Invalid parameters for 'initialize'",
                    None
                  )
                )
              )
            case Right(p) =>
              for result <- handleInitialize(
                  p.capabilities,
                  p.clientInfo,
                  p.protocolVersion
                )
              yield Right(result.asJson)
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
    "tools/call" -> { (params: JsonRpc.Params) =>
      params match
        case Params.ByPosition(values) =>
          // tools/call is not defined
          IO.pure(
            Left(
              JsonRpc.Error(
                JsonRpc.ErrorCode.MethodNotFound,
                "Method 'tools/call' is not defined for byPosition"
              )
            )
          )
        case Params.ByName(values) =>
          values.asJson.as[Method.CallTool] match
            case Left(value) =>
              IO.pure(
                Left(
                  JsonRpc.Error(
                    JsonRpc.ErrorCode.InvalidParams,
                    "Invalid parameters for 'tools/call'",
                    None
                  )
                )
              )
            case Right(p) =>
              for result <- handleCallTool(
                  p.name,
                  p.arguments
                )
              yield Right(result.asJson)
    }
    // 他のハンドラも同様に追加
  )

  // 古いエラーヘルパーと Map 定義は削除
}
