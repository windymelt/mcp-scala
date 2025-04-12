package example

import cats.effect.IO
import dev.capslock.mcpscala.mcp.ContentPart
import dev.capslock.mcpscala.McpIOApp
import sttp.tapir.Schema.annotations.description
import dev.capslock.mcpscala.Handler
import dev.capslock.mcpscala.server.Tool

case class RandomNumberInput(
    @description("Minimum value (inclusive)") min: Int,
    @description("Maximum value (exclusive)") max: Int
) derives io.circe.Decoder,
      sttp.tapir.Schema
def randomNumber(input: RandomNumberInput): IO[Seq[ContentPart]] = IO {
  val random = scala.util.Random.between(input.min, input.max)
  Seq(ContentPart.TextContentPart(random.toString))
}

case class IotaInput(
    @description("Initial bound") min: Int,
    @description("Final bound") max: Int
) derives io.circe.Decoder,
      sttp.tapir.Schema
def iota(input: IotaInput): IO[Seq[ContentPart]] = {
  val iota = (input.min to input.max).map(_.toString)
  IO.pure(Seq(ContentPart.TextContentPart(iota.mkString(","))))
}

case class SumInput(
    xs: Seq[Int]
) derives io.circe.Decoder,
      sttp.tapir.Schema
def sum(input: SumInput): IO[Seq[ContentPart]] = {
  val sum = input.xs.sum
  IO.pure(Seq(ContentPart.TextContentPart(sum.toString)))
}

object Main
    extends McpIOApp(
      name = "mcp-stdio-example",
      header = "MCP Stdio Example",
      version = None
    ):
  def handlers = Handler.methodHandlers(
    Map(
      "ping" -> Tool(
        () => Seq(ContentPart.TextContentPart("pong")),
        "Ping the server."
      ),
      "randomNumber" -> Tool(randomNumber, "Generate a random number."),
      "iota" -> Tool(
        iota,
        "Generate a sequence of numbers from min to max."
      ),
      "sum" -> Tool(
        sum,
        "Calculate the sum of a sequence of numbers."
      )
    )
  )
