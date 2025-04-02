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
import dev.capslock.mcpscala.web.JsonRpc._
import dev.capslock.mcpscala.web.JsonRpc.Error
import dev.capslock.mcpscala.web.Server
import io.circe._
import dev.capslock.mcpscala.mcp._
import MethodIsJsonRpc.{*, given}

/** 標準入出力を使用したJSONRPCサーバーのエントリーポイント
  */
object StdioMain extends IOApp.Simple {
  // StdioServerを使用してサーバーを起動
  def run: IO[Unit] = {
    StdioServer.serve(web.Handler.methodHandlers)
  }
}
