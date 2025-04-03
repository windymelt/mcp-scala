package dev.capslock.mcpscala.mcp

import io.circe.generic.semiauto
import io.circe.Codec

enum Method(val methodName: String) {
  case Initialize(
      capabilities: ClientCapabilities,
      clientInfo: Implementation,
      protocolVersion: String
  ) extends Method("initialize")

  case ListTools(
      cursor: Option[String]
  ) extends Method("tools/list")

  case CallTool(
      name: String,
      arguments: Option[
        Map[String, io.circe.Json]
      ] // arguments は JSON オブジェクトとして受け取る
  ) extends Method("tools/call")
}

// definition

case class ClientCapabilities( /* TODO */ )
case class ServerCapabilities( /* TODO */ tools: Option[Tools])
case class Tools()

case class Implementation(name: String, version: String)

case class InitializeResult(
    capabilities: ServerCapabilities,
    instructions: Option[String],
    protocolVersion: String,
    serverInfo: Implementation
)

case class ListToolsResult(
    tools: Seq[Tool]
)
case class Tool(
    inputSchema: io.circe.Json, // wip
    name: String
)
case class CallToolResult(isError: Boolean, content: Seq[ContentPart])
sealed trait ContentPart
case class TextContentPart(text: String, `type`: String = "text")
    extends ContentPart

object MethodIsJsonRpc {
  given Codec[Method] = semiauto.deriveCodec
  given Codec[ClientCapabilities] = semiauto.deriveCodec
  given Codec[ServerCapabilities] = semiauto.deriveCodec
  given Codec[Tools] = semiauto.deriveCodec
  given Codec[Implementation] = semiauto.deriveCodec
  given Codec[InitializeResult] = semiauto.deriveCodec
  given Codec[ListToolsResult] = semiauto.deriveCodec
  given Codec[Tool] = semiauto.deriveCodec
  given Codec[CallToolResult] = semiauto.deriveCodec
  given Codec[ContentPart] = semiauto.deriveCodec

  extension (methodJson: io.circe.Json)
    def asMessage: Either[io.circe.DecodingFailure, Method] =
      methodJson.as[Method]

}
