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
import com.comcast.ip4s._
import org.http4s.ember.server._
import org.http4s.implicits._
import org.http4s.server.Router
import dev.capslock.mcpscala.web.Server

// object Main extends IOApp.Simple {
//   val services = Server.jsonRpcService(Handler.methodHandlers)
//   val httpApp = Router("/" -> services).orNotFound
//   val server = EmberServerBuilder
//     .default[IO]
//     .withHost(ipv4"0.0.0.0")
//     .withPort(port"8080")
//     .withHttpApp(httpApp)
//     .build
//   def run: IO[Unit] =
//     server.useForever
// }
