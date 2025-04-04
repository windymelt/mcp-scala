package dev.capslock.mcpscala

import io.circe.syntax._
import cats.effect.IO
import dev.capslock.mcpscala.JsonRpc // JsonRpc.scala が同じパッケージにあると仮定
import dev.capslock.mcpscala.mcp.*
import dev.capslock.mcpscala.macros.HandlerMacros.{
  byNameHandler,
  //byPositionHandler,
} // 新しいマクロをインポート

object Handler {
  // MethodHandler と MethodHandlers の型エイリアスはマクロ利用側では不要になるかも
  type MethodHandler = JsonRpc.Params => IO[Either[JsonRpc.Error, io.circe.Json]]
  type MethodHandlers = Map[String, MethodHandler]

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
    "initialize" -> byNameHandler {
      // PartialFunction の case を使う
      case (
      protocolVersion: String,
      capabilities: ClientCapabilities,
            clientInfo: Implementation,
          ) =>
        handleInitialize(capabilities, clientInfo, protocolVersion)
    },
    // "tools/list" -> byPositionHandler {
    //   case cursor: Option[String] => // PartialFunctionに変更
    //     handleListTools(cursor)
    // },
    "tools/call" -> byNameHandler {
      case (name: String, arguments: Option[Map[String, io.circe.Json]]) =>
        handleCallTool(name, arguments)
    }
    // 他のハンドラも同様に追加
  )

  // 古いエラーヘルパーと Map 定義は削除
}
