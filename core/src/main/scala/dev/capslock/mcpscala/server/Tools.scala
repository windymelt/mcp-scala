package dev.capslock.mcpscala.server

import cats.effect.IO
import dev.capslock.mcpscala.mcp.ContentPart
import io.circe.Decoder
import sttp.tapir.Schema

object Tools {}

case class Tool[In](
    func: In => IO[Seq[ContentPart]],
    description: String = "(not provided)"
)(using val inputDecoder: Decoder[In], val tSchema: Schema[In]) {
  type Input = In
}
