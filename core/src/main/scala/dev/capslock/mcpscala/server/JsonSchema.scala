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

import io.circe.Printer
import io.circe.syntax.*
import sttp.apispec.circe.*
import sttp.apispec.{Schema => ASchema}
import sttp.tapir.*
import sttp.tapir.docs.apispec.schema.*
import sttp.tapir.generic.auto.*
import io.circe.Json

object JsonSchema {
  def genSchema[A](using tSchema: Schema[A]): Json = {
    val jsonSchema: ASchema =
      TapirSchemaToJsonSchema(tSchema, markOptionsAsNullable = true)

    jsonSchema.asJson
  }
}
