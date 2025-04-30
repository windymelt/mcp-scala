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

package dev.capslock.mcpscala.server

import cats.effect.IO
import dev.capslock.mcpscala.mcp.ContentPart
import io.circe.Decoder
import sttp.tapir.Schema

import scala.annotation.targetName

object Tools {}

case class Tool[In](
    func: In => IO[Seq[ContentPart]],
    description: String
)(using val inputDecoder: Decoder[In], val tSchema: Schema[In]) {
  type Input = In
}

object Tool {
  @targetName("pureApply")
  def apply[In](
      func: In => Seq[ContentPart],
      description: String
  )(using Decoder[In], Schema[In]): Tool[In] = {
    new Tool[In](in => IO.pure(func(in)), description)
  }

  def apply(
      func: () => IO[Seq[ContentPart]],
      description: String
  ): Tool[Unit] = {
    new Tool[Unit](_ => func(), description)
  }

  @targetName("pureApplyUnit")
  def apply(
      func: () => Seq[ContentPart],
      description: String
  ): Tool[Unit] = {
    new Tool[Unit](_ => IO.pure(func()), description)
  }
}
