/*
 * Copyright 2025 capslock.dev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
def randomNumber(input: RandomNumberInput): IO[Seq[ContentPart]] = {
  val random = scala.util.Random.between(input.min, input.max)
  IO.pure(Seq(ContentPart.TextContentPart(random.toString)))
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
    "randomNumber" -> server.Tool((input: RandomNumberInput) =>
      IO {
        val random = scala.util.Random.between(input.min, input.max)
        Seq(ContentPart.TextContentPart(random.toString))
      }
    ),
    "iota" -> server.Tool(
      (input: IotaInput) =>
        IO {
          val iota = (input.min to input.max).map(_.toString)
          Seq(ContentPart.TextContentPart(iota.mkString(",")))
        },
      "Generate a sequence of numbers from min to max."
    ),
    "sum" -> server.Tool(
      (input: SumInput) =>
        IO {
          val sum = input.xs.sum
          Seq(ContentPart.TextContentPart(sum.toString))
        },
      "Calculate the sum of a sequence of numbers."
    )
  )

  def run: IO[Unit] = {
    StdioServer.serve(Handler.methodHandlers(tools))
  }
}
