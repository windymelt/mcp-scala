# MCP-scala

Model Context Protocol server written in Scala 3

## Development stage

This software is currently ALPHA state.

- [x] Automatic derivation of JSON Schema
- [x] Define your tool
- [x] Text content part
- [ ] Other content parts
- [ ] Notification handling
- [ ] Capability handling
- [x] stdio transport
- [ ] HTTP transport
- [ ] Authorization feature

This implementation needs your attention and contribution.

Feel free to open Issue / PR to contribute this project.

## Demo

First, build server into JS:

```sh
sbt example/fastLinkJS
```

Then, utilize server in your MCP client:

```json
// Example for Cline
{
  "mcpServers": {
    "mcpscala": {
      "disabled": false,
      "timeout": 30,
      "command": "sh",
      "args": ["/path/to/run.sh"],
      "transportType": "stdio"
    }
  }
}
```

You can run some tool:

- `randomNumber`
  - generate random number between `min` to `max`.
- `iota`
  - generate a sequence of numbers from `min` to `max`.
- `sum`
  - calculate the sum of a sequence of numbers.

## Implement your tool

See `Main.scala` for details.

```scala
//> using scala 3
//> using toolkit typelevel:default
//> using dep dev.capslock::mcpscala:0.1.0

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

/** Entry point for the Stdio server.
  */
object StdioMain extends McpIOApp(
    name = "random-number",
    header = "Generate a random number",
  ):
    
  val handlers = Handler.methodHandlers(
    Map(
      "randomNumber" -> server.Tool(randomNumber),
    )
  )

```
