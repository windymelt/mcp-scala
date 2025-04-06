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
package transport

import cats.effect._
import fs2._
import fs2.io.stdin
import fs2.io.stdout
import Handler.MethodHandlers

object StdioServer {

  /** サーバーを起動し、標準入出力でJSONRPCリクエストを処理する
    *
    * @param methodHandlers
    *   登録するメソッドハンドラー
    * @return
    *   実行完了を表すIO
    */
  def serve(methodHandlers: MethodHandlers[IO]): IO[Unit] = {
    // 標準入力からリクエストを読み込み処理し、標準出力にレスポンスを書き込むパイプライン
    stdin[IO](4096)
      .through(text.utf8.decode)
      .through(text.lines)
      .evalMap(Server.handleJsonRpcRequest(methodHandlers))
      // 改行付きでレスポンスを出力
      .map(result => s"$result\n")
      .through(text.utf8.encode)
      .through(stdout[IO])
      .compile
      .drain
  }
}
