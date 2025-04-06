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
