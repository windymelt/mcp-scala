package dev.capslock.mcpscala.mcp

import io.circe.generic.semiauto
import io.circe.{Codec, Decoder, Encoder}
import io.circe.derivation.Configuration
import io.circe.derivation.ConfiguredEncoder

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
    name: String,
    description: String
)
case class CallToolResult(isError: Boolean, content: Seq[ContentPart])
    derives Encoder

object ContentPart {
  given Configuration = Configuration.default
    .withDiscriminator("type")
    .withTransformConstructorNames { case "TextContentPart" =>
      "text"
    }
}
enum ContentPart derives ConfiguredEncoder {
  case TextContentPart(text: String, `type`: "text" = "text")
}

object MethodIsJsonRpc {
  given Codec[Method] = semiauto.deriveCodec
  given Decoder[Method.Initialize] = semiauto.deriveDecoder
  given Decoder[Method.ListTools] = semiauto.deriveDecoder
  given Decoder[Method.CallTool] = semiauto.deriveDecoder
  given Codec[ClientCapabilities] = semiauto.deriveCodec
  given Codec[ServerCapabilities] = semiauto.deriveCodec
  given Codec[Tools] = semiauto.deriveCodec
  given Codec[Implementation] = semiauto.deriveCodec
  given Codec[InitializeResult] = semiauto.deriveCodec
  given Codec[ListToolsResult] = semiauto.deriveCodec
  given Codec[Tool] = semiauto.deriveCodec

  extension (methodJson: io.circe.Json)
    def asMessage: Either[io.circe.DecodingFailure, Method] =
      methodJson.as[Method]

}
