package dev.capslock.mcpscala.server

import cats.effect.IO
import dev.capslock.mcpscala.mcp.ContentPart
import io.circe.Decoder

object Tools {}

case class Tool[In](
    inputSchema: io.circe.Json,
    func: In => IO[Seq[ContentPart]]
)(using val inputDecoder: Decoder[In]) {
  type Input = In
}
