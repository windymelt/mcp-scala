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
import dev.capslock.mcpscala.web.StdioServer
import dev.capslock.mcpscala.mcp.ContentPart

case class RandomNumberInput(min: Int, max: Int) derives io.circe.Decoder
def randomNumber(input: RandomNumberInput): IO[Seq[ContentPart]] = {
  val random = scala.util.Random.between(input.min, input.max)
  IO.pure(Seq(ContentPart.TextContentPart(random.toString)))
}

/** 標準入出力を使用したJSONRPCサーバーのエントリーポイント
  */
object StdioMain extends IOApp.Simple {
  import io.circe.*
  val tools = Map(
    "randomNumber" -> server.Tool(
      Json.obj(
        "type" -> Json.fromString("object"),
        "properties" -> Json.obj(
          "min" -> Json.obj("type" -> Json.fromString("number")),
          "max" -> Json.obj("type" -> Json.fromString("number"))
        ),
        "required" -> Json
          .arr(Json.fromString("min"), Json.fromString("max"))
      ),
      (input: RandomNumberInput) =>
        IO {
          val random = scala.util.Random.between(input.min, input.max)
          Seq(ContentPart.TextContentPart(random.toString))
        }
    )
  )
  def run: IO[Unit] = {
    StdioServer.serve(Handler.methodHandlers(tools))
  }
}
