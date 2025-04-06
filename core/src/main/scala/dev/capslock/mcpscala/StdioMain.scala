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

package dev.capslock.mcpscala

import cats.effect.IO
import cats.effect.IOApp
import dev.capslock.mcpscala.transport.StdioServer
import dev.capslock.mcpscala.mcp.ContentPart
import sttp.tapir.Schema.annotations.description

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

/** Entry point for the Stdio server.
  */
object StdioMain extends IOApp.Simple {
  val tools = Map(
    "randomNumber" -> server.Tool(randomNumber),
    "iota" -> server.Tool(
      iota,
      "Generate a sequence of numbers from min to max."
    ),
    "sum" -> server.Tool(
      sum,
      "Calculate the sum of a sequence of numbers."
    )
  )

  def run: IO[Unit] = {
    StdioServer.serve(Handler.methodHandlers(tools))
  }
}
