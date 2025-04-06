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
package transport

import cats.effect._
import fs2._
import fs2.io.stdin
import fs2.io.stdout
import Handler.MethodHandlers

object StdioServer {

  /** Provide a JSON-RPC server using standard input and output.
    */
  def serve(methodHandlers: MethodHandlers[IO]): IO[Unit] = {
    stdin[IO](4096)
      .through(text.utf8.decode)
      .through(text.lines)
      .evalMap(Server.handleJsonRpcRequest(methodHandlers))
      .map(result => s"$result\n")
      .through(text.utf8.encode)
      .through(stdout[IO])
      .compile
      .drain
  }
}
