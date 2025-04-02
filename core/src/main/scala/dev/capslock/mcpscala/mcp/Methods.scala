package dev.capslock.mcpscala.mcp

import io.circe.generic.semiauto
import io.circe.Codec

enum Method(val methodName: String) {
  case Initialize(
      capabilities: ClientCapabilities,
      clientInfo: Implementation,
      protocolVersion: String
  ) extends Method("initialize")
}

// definition

case class ClientCapabilities( /* TODO */ )
case class ServerCapabilities( /* TODO */ )

case class Implementation(name: String, version: String)

case class InitializeResult(
    capabilities: ServerCapabilities,
    instructions: Option[String],
    protocolVersion: String,
    serverInfo: Implementation
)

object MethodIsJsonRpc {
  given Codec[Method] = semiauto.deriveCodec
  given Codec[ClientCapabilities] = semiauto.deriveCodec
  given Codec[ServerCapabilities] = semiauto.deriveCodec
  given Codec[Implementation] = semiauto.deriveCodec
  given Codec[InitializeResult] = semiauto.deriveCodec

  extension (methodJson: io.circe.Json)
    def asMessage: Either[io.circe.DecodingFailure, Method] =
      methodJson.as[Method]

}
