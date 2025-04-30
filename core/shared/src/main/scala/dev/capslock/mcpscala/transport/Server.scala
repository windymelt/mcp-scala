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

import cats.effect.*
import cats.syntax.all.*
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

import Handler.MethodHandlers

object Server {
  // to remove ambiguity, placing this import here
  import JsonRpc._

  def handleJsonRpcRequest(
      methodHandlers: MethodHandlers[IO]
  )(body: String): IO[String] = {
    import JsonRpc.Codec.given

    parse(body) match {
      case Left(error) =>
        val response: Response = Response.Failure(
          Error(ErrorCode.ParseError, s"Invalid JSON: ${error.message}"),
          RequestId.NullId
        )
        IO.pure(response.asJson.noSpaces)
      case Right(json) =>
        json.as[Request] match {
          case Left(error) =>
            val response: Response = Response.Failure(
              Error(
                ErrorCode.InvalidRequest,
                s"Invalid Request: ${error.message}"
              ),
              RequestId.NullId
            )
            IO.pure(response.asJson.noSpaces)
          case Right(request) =>
            processRequest(methodHandlers)(request).map(_.asJson.noSpaces)
        }
    }
  }

  def processRequest(
      methodHandlers: MethodHandlers[IO]
  )(request: Request): IO[Response] = {
    request match {
      case call @ Request.Call(_, _, _) =>
        processCallRequest(methodHandlers)(call)
      case notification @ Request.Notification(_, _) =>
        processNotificationRequest(methodHandlers)(notification)
      case batch @ Request.Batch(_) =>
        processBatchRequest(methodHandlers)(batch)
    }
  }

  private def processCallRequest(
      methodHandlers: MethodHandlers[IO]
  )(call: Request.Call): IO[Response] = {
    val Request.Call(method, params, id) = call
    methodHandlers.get(method) match {
      case Some(handler) =>
        params match {
          case Some(p) =>
            handler(p).map {
              case Right(result) => Response.Success(result, id)
              case Left(error)   => Response.Failure(error, id)
            }
          case None =>
            handler(Params.ByPosition(List())).map {
              case Right(result) => Response.Success(result, id)
              case Left(error)   => Response.Failure(error, id)
            }
        }
      case None =>
        IO.pure(
          Response.Failure(
            Error(
              ErrorCode.MethodNotFound,
              s"Method not found: $method"
            ),
            id
          )
        )
    }
  }

  private def processNotificationRequest(
      methodHandlers: MethodHandlers[IO]
  )(notification: Request.Notification): IO[Response] = {
    val Request.Notification(method, params) = notification
    val processEffect = methodHandlers.get(method) match {
      case Some(handler) =>
        params match {
          case Some(p) => handler(p)
          case None    => handler(Params.ByPosition(List()))
        }
      case None => IO.pure(Right(Json.Null))
    }

    processEffect.map(_ =>
      Response.Success(
        Json.fromString("Notification processed"),
        RequestId.NullId
      )
    )
  }

  private def processBatchRequest(
      methodHandlers: MethodHandlers[IO]
  )(batch: Request.Batch): IO[Response] = {
    val Request.Batch(requests) = batch
    if (requests.isEmpty) {
      IO.pure(
        Response.Failure(
          Error(ErrorCode.InvalidRequest, "Empty batch"),
          RequestId.NullId
        )
      )
    } else {
      val responseIOs = requests.map {
        case call @ Request.Call(_, _, _) =>
          processCallRequest(methodHandlers)(call).map(Some(_))
        case notification @ Request.Notification(_, _) =>
          processNotificationRequest(methodHandlers)(notification).map(_ =>
            None
          )
        case _ =>
          IO.pure(
            Some(
              Response.Failure(
                Error(
                  ErrorCode.InvalidRequest,
                  "Nested batches not supported"
                ),
                RequestId.NullId
              )
            )
          )
      }

      responseIOs.sequence.map { optResponses =>
        val validResponses = optResponses.flatten
        if (validResponses.isEmpty) {
          Response.Success(
            Json.fromString("All notifications processed"),
            RequestId.NullId
          )
        } else {
          Response.BatchResponse(validResponses.toList)
        }
      }
    }
  }
}
