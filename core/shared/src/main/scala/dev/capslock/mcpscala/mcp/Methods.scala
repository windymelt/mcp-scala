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

package dev.capslock.mcpscala.mcp

import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.Configuration
import io.circe.derivation.ConfiguredEncoder
import io.circe.generic.semiauto

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
