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

package dev.capslock.mcpscala.web

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import io.circe._
import io.circe.parser._
import io.circe.syntax._

class JsonRpcSpec extends AnyFunSpec with Matchers:
  import JsonRpc._
  import JsonRpc.Codec.given

  describe("RequestId") {
    it("should encode/decode StringId") {
      val id = RequestId.StringId("test-id")
      val json = id.asJson
      json shouldBe Json.fromString("test-id")
      json.as[RequestId].toOption.get shouldBe id
    }

    it("should encode/decode NumberId") {
      val id = RequestId.NumberId(123)
      val json = id.asJson
      json shouldBe Json.fromInt(123)
      json.as[RequestId].toOption.get shouldBe id
    }

    it("should encode/decode NullId") {
      val id = RequestId.NullId
      val json = id.asJson
      json shouldBe Json.Null
      json.as[RequestId].toOption.get shouldBe id
    }
  }

  describe("Params") {
    it("should encode/decode ByPosition") {
      val params =
        Params.ByPosition(List(Json.fromInt(1), Json.fromString("test")))
      val json = params.asJson
      json shouldBe Json.arr(Json.fromInt(1), Json.fromString("test"))
      json.as[Params].toOption.get shouldBe params
    }

    it("should encode/decode ByName") {
      val params = Params.ByName(
        Map("a" -> Json.fromInt(1), "b" -> Json.fromString("test"))
      )
      val json = params.asJson
      json.as[Params].toOption.get shouldBe params
    }
  }

  describe("ErrorCode") {
    it("should encode/decode standard error codes") {
      val codes = List(
        ErrorCode.ParseError,
        ErrorCode.InvalidRequest,
        ErrorCode.MethodNotFound,
        ErrorCode.InvalidParams,
        ErrorCode.InternalError
      )

      for code <- codes do
        val json = code.asJson
        json shouldBe Json.fromInt(code.code)
        json.as[ErrorCode].toOption.get shouldBe code
    }

    it("should encode/decode ServerError") {
      val code = ErrorCode.ServerError(-32050)
      val json = code.asJson
      json shouldBe Json.fromInt(-32050)
      json.as[ErrorCode].toOption.get shouldBe code
    }

    it("should encode/decode ApplicationError") {
      val code = ErrorCode.ApplicationError(-1)
      val json = code.asJson
      json shouldBe Json.fromInt(-1)
      json.as[ErrorCode].toOption.get shouldBe code
    }

    it("should validate error codes") {
      ErrorCode.ParseError.isValid shouldBe true
      ErrorCode.ServerError(-32000).isValid shouldBe true
      ErrorCode.ServerError(-32099).isValid shouldBe true
      ErrorCode.ServerError(-32100).isValid shouldBe false
      ErrorCode.ApplicationError(-1).isValid shouldBe true
      ErrorCode.ApplicationError(-32000).isValid shouldBe false
      ErrorCode.ApplicationError(-32768).isValid shouldBe false
    }
  }

  describe("Error") {
    it("should encode/decode basic error") {
      val error = Error(ErrorCode.InvalidRequest, "Invalid request")
      val json = error.asJson
      json.hcursor.get[Int]("code").toOption.get shouldBe -32600
      json.hcursor
        .get[String]("message")
        .toOption
        .get shouldBe "Invalid request"
      json.hcursor.get[Option[Json]]("data").toOption.get shouldBe None
      json.as[Error].toOption.get shouldBe error
    }

    it("should encode/decode error with data") {
      val data = Json.obj("detail" -> Json.fromString("More information"))
      val error =
        Error(ErrorCode.InvalidParams, "Invalid parameters", Some(data))
      val json = error.asJson
      json.hcursor.get[Int]("code").toOption.get shouldBe -32602
      json.hcursor
        .get[String]("message")
        .toOption
        .get shouldBe "Invalid parameters"
      json.hcursor.get[Option[Json]]("data").toOption.get shouldBe Some(data)
      json.as[Error].toOption.get shouldBe error
    }
  }

  describe("Request") {
    it("should encode/decode Call request") {
      val params = Params.ByPosition(List(Json.fromInt(1), Json.fromInt(2)))
      val request = Request.Call("add", Some(params), RequestId.NumberId(1))
      val json = request.asJson
      json.hcursor.get[String]("jsonrpc").toOption.get shouldBe "2.0"
      json.hcursor.get[String]("method").toOption.get shouldBe "add"
      json.hcursor.downField("params").as[Json].toOption.get shouldBe Json.arr(
        Json.fromInt(1),
        Json.fromInt(2)
      )
      json.hcursor.get[Int]("id").toOption.get shouldBe 1
      json.as[Request].toOption.get shouldBe request
    }

    it("should encode/decode Notification request") {
      val params = Params.ByPosition(List(Json.fromInt(1), Json.fromInt(2)))
      val request = Request.Notification("update", Some(params))
      val json = request.asJson
      json.hcursor.get[String]("jsonrpc").toOption.get shouldBe "2.0"
      json.hcursor.get[String]("method").toOption.get shouldBe "update"
      json.hcursor.downField("params").as[Json].toOption.get shouldBe Json.arr(
        Json.fromInt(1),
        Json.fromInt(2)
      )
      json.hcursor.downField("id").focus shouldBe None
      json.as[Request].toOption.get shouldBe request
    }

    it("should encode/decode Batch request") {
      val request1 = Request.Call(
        "add",
        Some(Params.ByPosition(List(Json.fromInt(1), Json.fromInt(2)))),
        RequestId.NumberId(1)
      )
      val request2 = Request.Notification("update", None)
      val batch = Request.Batch(List(request1, request2))
      val json = batch.asJson
      json.asArray.get.size shouldBe 2
      json.as[Request].toOption.get shouldBe batch
    }
  }

  describe("Response") {
    it("should encode/decode Success response") {
      val response = Response.Success(Json.fromInt(42), RequestId.NumberId(1))
      val json = response.asJson
      json.hcursor.get[String]("jsonrpc").toOption.get shouldBe "2.0"
      json.hcursor.get[Int]("result").toOption.get shouldBe 42
      json.hcursor.get[Int]("id").toOption.get shouldBe 1
      json.as[Response].toOption.get shouldBe response
    }

    it("should encode/decode Failure response") {
      val error = Error(ErrorCode.MethodNotFound, "Method not found")
      val response = Response.Failure(error, RequestId.StringId("1"))
      val json = response.asJson
      json.hcursor.get[String]("jsonrpc").toOption.get shouldBe "2.0"
      json.hcursor
        .downField("error")
        .get[Int]("code")
        .toOption
        .get shouldBe -32601
      json.hcursor
        .downField("error")
        .get[String]("message")
        .toOption
        .get shouldBe "Method not found"
      json.hcursor.get[String]("id").toOption.get shouldBe "1"
      json.as[Response].toOption.get shouldBe response
    }

    it("should encode/decode BatchResponse") {
      val response1 = Response.Success(Json.fromInt(42), RequestId.NumberId(1))
      val response2 = Response.Failure(
        Error(ErrorCode.InvalidParams, "Invalid params"),
        RequestId.StringId("2")
      )
      val batchResponse = Response.BatchResponse(List(response1, response2))
      val json = batchResponse.asJson
      json.asArray.get.size shouldBe 2
      json.as[Response].toOption.get shouldBe batchResponse
    }
  }

  describe("JSON-RPC 2.0 Specification Examples") {
    it("should handle positional parameters example") {
      val jsonStr =
        """{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}"""
      val json = parse(jsonStr).getOrElse(Json.Null)

      val request = json.as[Request].toOption.get
      request shouldBe a[Request.Call]

      val call = request.asInstanceOf[Request.Call]
      call.method shouldBe "subtract"
      call.params.get shouldBe a[Params.ByPosition]
      call.params.get.asInstanceOf[Params.ByPosition].values shouldBe List(
        Json.fromInt(42),
        Json.fromInt(23)
      )
      call.id shouldBe RequestId.NumberId(1)

      val responseJson = """{"jsonrpc": "2.0", "result": 19, "id": 1}"""
      val response =
        parse(responseJson).getOrElse(Json.Null).as[Response].toOption.get
      response shouldBe a[Response.Success]
      response.asInstanceOf[Response.Success].result shouldBe Json.fromInt(19)
      response.asInstanceOf[Response.Success].id shouldBe RequestId.NumberId(1)
    }

    it("should handle named parameters example") {
      val jsonStr =
        """{"jsonrpc": "2.0", "method": "subtract", "params": {"subtrahend": 23, "minuend": 42}, "id": 3}"""
      val json = parse(jsonStr).getOrElse(Json.Null)

      val request = json.as[Request].toOption.get
      request shouldBe a[Request.Call]

      val call = request.asInstanceOf[Request.Call]
      call.method shouldBe "subtract"
      call.params.get shouldBe a[Params.ByName]

      val params = call.params.get.asInstanceOf[Params.ByName].values
      params("subtrahend") shouldBe Json.fromInt(23)
      params("minuend") shouldBe Json.fromInt(42)

      call.id shouldBe RequestId.NumberId(3)
    }

    it("should handle notification example") {
      val jsonStr =
        """{"jsonrpc": "2.0", "method": "update", "params": [1,2,3,4,5]}"""
      val json = parse(jsonStr).getOrElse(Json.Null)

      val request = json.as[Request].toOption.get
      request shouldBe a[Request.Notification]

      val notification = request.asInstanceOf[Request.Notification]
      notification.method shouldBe "update"
      notification.params.get shouldBe a[Params.ByPosition]

      val params =
        notification.params.get.asInstanceOf[Params.ByPosition].values
      params shouldBe List(
        Json.fromInt(1),
        Json.fromInt(2),
        Json.fromInt(3),
        Json.fromInt(4),
        Json.fromInt(5)
      )
    }

    it("should handle non-existent method example") {
      val jsonStr = """{"jsonrpc": "2.0", "method": "foobar", "id": "1"}"""
      val json = parse(jsonStr).getOrElse(Json.Null)

      val request = json.as[Request].toOption.get
      request shouldBe a[Request.Call]

      val call = request.asInstanceOf[Request.Call]
      call.method shouldBe "foobar"
      call.params shouldBe None
      call.id shouldBe RequestId.StringId("1")

      val responseJson =
        """{"jsonrpc": "2.0", "error": {"code": -32601, "message": "Method not found"}, "id": "1"}"""
      val response =
        parse(responseJson).getOrElse(Json.Null).as[Response].toOption.get
      response shouldBe a[Response.Failure]

      val failure = response.asInstanceOf[Response.Failure]
      failure.error.code shouldBe ErrorCode.MethodNotFound
      failure.error.message shouldBe "Method not found"
      failure.id shouldBe RequestId.StringId("1")
    }

    it("should handle invalid request object example") {
      val jsonStr = """{"jsonrpc": "2.0", "method": 1, "params": "bar"}"""
      val json = parse(jsonStr).getOrElse(Json.Null)

      val result = json.as[Request]
      result.isLeft shouldBe true

      val responseJson =
        """{"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid Request"}, "id": null}"""
      val response =
        parse(responseJson).getOrElse(Json.Null).as[Response].toOption.get
      response shouldBe a[Response.Failure]

      val failure = response.asInstanceOf[Response.Failure]
      failure.error.code shouldBe ErrorCode.InvalidRequest
      failure.error.message shouldBe "Invalid Request"
      failure.id shouldBe RequestId.NullId
    }

    it("should handle batch request example") {
      val jsonStr = """[
        {"jsonrpc": "2.0", "method": "sum", "params": [1,2,4], "id": "1"},
        {"jsonrpc": "2.0", "method": "notify_hello", "params": [7]},
        {"jsonrpc": "2.0", "method": "subtract", "params": [42,23], "id": "2"}
      ]"""
      val json = parse(jsonStr).getOrElse(Json.Null)

      val request = json.as[Request].toOption.get
      request shouldBe a[Request.Batch]

      val batch = request.asInstanceOf[Request.Batch]
      batch.requests.size shouldBe 3

      batch.requests(0) shouldBe a[Request.Call]
      batch.requests(0).asInstanceOf[Request.Call].method shouldBe "sum"

      batch.requests(1) shouldBe a[Request.Notification]
      batch
        .requests(1)
        .asInstanceOf[Request.Notification]
        .method shouldBe "notify_hello"

      batch.requests(2) shouldBe a[Request.Call]
      batch.requests(2).asInstanceOf[Request.Call].method shouldBe "subtract"
    }

    it("should handle empty batch request example") {
      val jsonStr = "[]"
      val json = parse(jsonStr).getOrElse(Json.Null)

      val result = json.as[Request]
      result.isLeft shouldBe true

      val responseJson =
        """{"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid Request"}, "id": null}"""
      val response =
        parse(responseJson).getOrElse(Json.Null).as[Response].toOption.get
      response shouldBe a[Response.Failure]

      val failure = response.asInstanceOf[Response.Failure]
      failure.error.code shouldBe ErrorCode.InvalidRequest
      failure.id shouldBe RequestId.NullId
    }

    it("should handle invalid batch request example") {
      val jsonStr = "[1]"
      val json = parse(jsonStr).getOrElse(Json.Null)

      val result = json.as[Request]
      result.isLeft shouldBe true
    }

    it("should handle notification-only batch request example") {
      val jsonStr = """[
        {"jsonrpc": "2.0", "method": "notify_sum", "params": [1,2,4]},
        {"jsonrpc": "2.0", "method": "notify_hello", "params": [7]}
      ]"""
      val json = parse(jsonStr).getOrElse(Json.Null)

      val request = json.as[Request].toOption.get
      request shouldBe a[Request.Batch]

      val batch = request.asInstanceOf[Request.Batch]
      batch.requests.size shouldBe 2
      batch.requests.forall(_.isInstanceOf[Request.Notification]) shouldBe true
    }
  }
