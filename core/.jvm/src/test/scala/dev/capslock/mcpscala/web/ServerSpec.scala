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
package web

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe._

class ServerSpec extends BaseSpec:
  import JsonRpc._
  import Server._

  describe("processRequest") {
    val testMethodHandlers: MethodHandlers = Map(
      "echo" -> { params =>
        IO.pure(
          params match {
            case Params.ByPosition(values) if values.nonEmpty =>
              Right(values.head)
            case Params.ByName(values) if values.nonEmpty =>
              Right(values.values.head)
            case _ =>
              Left(
                Error(
                  ErrorCode.InvalidParams,
                  "Expected at least one parameter"
                )
              )
          }
        )
      },
      "add" -> { params =>
        IO.pure(
          params match {
            case Params.ByPosition(values) if values.length >= 2 =>
              (values(0).asNumber, values(1).asNumber) match {
                case (Some(a), Some(b)) =>
                  Right(
                    Json.fromBigDecimal(
                      a.toBigDecimal.getOrElse(BigDecimal(0)) + b.toBigDecimal
                        .getOrElse(BigDecimal(0))
                    )
                  )
                case _ =>
                  Left(
                    Error(ErrorCode.InvalidParams, "Parameters must be numbers")
                  )
              }
            case _ =>
              Left(
                Error(
                  ErrorCode.InvalidParams,
                  "Expected two numeric parameters"
                )
              )
          }
        )
      }
    )

    it("should process a Call request") {
      val request = Request.Call(
        "echo",
        Some(Params.ByPosition(List(Json.fromString("test")))),
        RequestId.NumberId(1)
      )

      val response = processRequest(testMethodHandlers)(request).unsafeRunSync()

      response shouldBe a[Response.Success]
      val success = response.asInstanceOf[Response.Success]
      success.result shouldBe Json.fromString("test")
      success.id shouldBe RequestId.NumberId(1)
    }

    it("should process a Call request with addition") {
      val request = Request.Call(
        "add",
        Some(Params.ByPosition(List(Json.fromInt(2), Json.fromInt(3)))),
        RequestId.StringId("test-id")
      )

      val response = processRequest(testMethodHandlers)(request).unsafeRunSync()

      response shouldBe a[Response.Success]
      val success = response.asInstanceOf[Response.Success]
      success.result shouldBe Json.fromInt(5)
      success.id shouldBe RequestId.StringId("test-id")
    }

    it("should handle method not found error") {
      val request = Request.Call(
        "non_existent_method",
        None,
        RequestId.NumberId(1)
      )

      val response = processRequest(testMethodHandlers)(request).unsafeRunSync()

      response shouldBe a[Response.Failure]
      val failure = response.asInstanceOf[Response.Failure]
      failure.error.code shouldBe ErrorCode.MethodNotFound
      failure.id shouldBe RequestId.NumberId(1)
    }

    it("should process a Notification request") {
      val request = Request.Notification(
        "echo",
        Some(Params.ByPosition(List(Json.fromString("test"))))
      )

      val response = processRequest(testMethodHandlers)(request).unsafeRunSync()

      response shouldBe a[Response.Success]
      val success = response.asInstanceOf[Response.Success]
      success.result shouldBe Json.fromString("Notification processed")
      success.id shouldBe RequestId.NullId
    }

    it("should process a Batch request") {
      val request = Request.Batch(
        List(
          Request.Call(
            "echo",
            Some(Params.ByPosition(List(Json.fromString("test1")))),
            RequestId.NumberId(1)
          ),
          Request.Call(
            "add",
            Some(Params.ByPosition(List(Json.fromInt(2), Json.fromInt(3)))),
            RequestId.NumberId(2)
          ),
          Request.Notification(
            "echo",
            Some(Params.ByPosition(List(Json.fromString("test3"))))
          )
        )
      )

      val response = processRequest(testMethodHandlers)(request).unsafeRunSync()

      response shouldBe a[Response.BatchResponse]
      val batchResponse = response.asInstanceOf[Response.BatchResponse]
      batchResponse.responses.size shouldBe 2 // 2 calls, 1 notification (not included in response)

      batchResponse.responses(0) shouldBe a[Response.Success]
      val success1 = batchResponse.responses(0).asInstanceOf[Response.Success]
      success1.result shouldBe Json.fromString("test1")
      success1.id shouldBe RequestId.NumberId(1)

      batchResponse.responses(1) shouldBe a[Response.Success]
      val success2 = batchResponse.responses(1).asInstanceOf[Response.Success]
      success2.result shouldBe Json.fromInt(5)
      success2.id shouldBe RequestId.NumberId(2)
    }

    it("should handle empty batch request") {
      val request = Request.Batch(List())

      val response = processRequest(testMethodHandlers)(request).unsafeRunSync()

      response shouldBe a[Response.Failure]
      val failure = response.asInstanceOf[Response.Failure]
      failure.error.code shouldBe ErrorCode.InvalidRequest
      failure.error.message shouldBe "Empty batch"
      failure.id shouldBe RequestId.NullId
    }

    it("should handle notification-only batch request") {
      val request = Request.Batch(
        List(
          Request.Notification(
            "echo",
            Some(Params.ByPosition(List(Json.fromString("test1"))))
          ),
          Request.Notification(
            "echo",
            Some(Params.ByPosition(List(Json.fromString("test2"))))
          )
        )
      )

      val response = processRequest(testMethodHandlers)(request).unsafeRunSync()

      response shouldBe a[Response.Success]
      val success = response.asInstanceOf[Response.Success]
      success.result shouldBe Json.fromString("All notifications processed")
      success.id shouldBe RequestId.NullId
    }
  }
