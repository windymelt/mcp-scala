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

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*

object JsonRpc:
  val Version = "2.0"

  enum RequestId:
    case StringId(value: String)
    case NumberId(value: BigDecimal)
    case NullId

  enum Params:
    case ByPosition(values: List[Json])
    case ByName(values: Map[String, Json])

  enum Request:
    case Call(method: String, params: Option[Params], id: RequestId)
    case Notification(method: String, params: Option[Params])
    case Batch(requests: List[Request])

  enum ErrorCode(val code: Int):
    case ParseError extends ErrorCode(-32700)
    case InvalidRequest extends ErrorCode(-32600)
    case MethodNotFound extends ErrorCode(-32601)
    case InvalidParams extends ErrorCode(-32602)
    case InternalError extends ErrorCode(-32603)
    case ServerError(override val code: Int) extends ErrorCode(code)
    case ApplicationError(override val code: Int) extends ErrorCode(code)

    def isValid: Boolean = this match
      case ServerError(c)      => c >= -32099 && c <= -32000
      case ApplicationError(c) => c < -32768 || c > -32000
      case _                   => true

  case class Error(
      code: ErrorCode,
      message: String,
      data: Option[Json] = None
  )

  enum Response:
    case Success(result: Json, id: RequestId)
    case Failure(error: Error, id: RequestId)
    case BatchResponse(responses: List[Response])

  object Codec:
    given Encoder[RequestId] = Encoder.instance {
      case RequestId.StringId(value) => Json.fromString(value)
      case RequestId.NumberId(value) => Json.fromBigDecimal(value)
      case RequestId.NullId          => Json.Null
    }

    given Decoder[RequestId] = Decoder.instance { cursor =>
      cursor.focus match
        case Some(Json.Null) => Right(RequestId.NullId)
        case Some(json) if json.isString =>
          json.asString
            .map(RequestId.StringId.apply)
            .toRight(DecodingFailure("Expected string", cursor.history))
        case Some(json) if json.isNumber =>
          json.asNumber
            .flatMap(_.toBigDecimal)
            .map(RequestId.NumberId.apply)
            .toRight(DecodingFailure("Expected number", cursor.history))
        case _ =>
          Left(
            DecodingFailure("Expected string, number or null", cursor.history)
          )
    }

    given Encoder[Params] = Encoder.instance {
      case Params.ByPosition(values) => Json.arr(values*)
      case Params.ByName(values)     => Json.obj(values.toSeq*)
    }

    given Decoder[Params] = Decoder.instance { cursor =>
      cursor.focus match
        case Some(json) if json.isArray =>
          json.asArray
            .map(arr => Params.ByPosition(arr.toList))
            .toRight(DecodingFailure("Expected array", cursor.history))
        case Some(json) if json.isObject =>
          json.asObject
            .map(obj => Params.ByName(obj.toMap))
            .toRight(DecodingFailure("Expected object", cursor.history))
        case _ =>
          Left(DecodingFailure("Expected array or object", cursor.history))
    }

    given Encoder[ErrorCode] = Encoder.instance(ec => Json.fromInt(ec.code))

    given Decoder[ErrorCode] = Decoder.instance { cursor =>
      cursor.as[Int].flatMap { code =>
        code match
          case -32700 => Right(ErrorCode.ParseError)
          case -32600 => Right(ErrorCode.InvalidRequest)
          case -32601 => Right(ErrorCode.MethodNotFound)
          case -32602 => Right(ErrorCode.InvalidParams)
          case -32603 => Right(ErrorCode.InternalError)
          case c if c >= -32099 && c <= -32000 =>
            Right(ErrorCode.ServerError(c))
          case c => Right(ErrorCode.ApplicationError(c))
      }
    }

    given Encoder[Error] = deriveEncoder[Error]
    given Decoder[Error] = deriveDecoder[Error]

    given requestEncoder: Encoder[Request] = Encoder.instance {
      case Request.Call(method, params, id) =>
        Json
          .obj(
            "jsonrpc" -> Json.fromString(Version),
            "method" -> Json.fromString(method),
            "id" -> id.asJson
          )
          .deepMerge(
            params.fold(Json.obj())(p => Json.obj("params" -> p.asJson))
          )

      case Request.Notification(method, params) =>
        Json
          .obj(
            "jsonrpc" -> Json.fromString(Version),
            "method" -> Json.fromString(method)
          )
          .deepMerge(
            params.fold(Json.obj())(p => Json.obj("params" -> p.asJson))
          )

      case Request.Batch(requests) =>
        Json.arr(requests.map(_.asJson(using requestEncoder))*)
    }

    given requestDecoder: Decoder[Request] = Decoder.instance { cursor =>
      if cursor.focus.exists(_.isArray) then
        cursor.as[List[Json]].flatMap { jsons =>
          if jsons.isEmpty then
            Left(DecodingFailure("Empty batch", cursor.history))
          else
            val decodedRequests = jsons.map(_.as[Request](using requestDecoder))
            val errors = decodedRequests.collect { case Left(e) => e }
            if errors.nonEmpty then Left(errors.head)
            else
              Right(Request.Batch(decodedRequests.collect { case Right(r) =>
                r
              }))
        }
      else
        for
          jsonrpc <- cursor.get[String]("jsonrpc")
          _ <-
            if jsonrpc == Version then Right(())
            else
              Left(
                DecodingFailure(
                  s"Expected jsonrpc version $Version",
                  cursor.history
                )
              )
          method <- cursor.get[String]("method")
          params <- cursor.get[Option[Params]]("params")
          idCursor = cursor.downField("id")
          hasId = idCursor.focus.isDefined
        yield
          if hasId then
            Request.Call(
              method,
              params,
              idCursor.as[RequestId].getOrElse(RequestId.NullId)
            )
          else Request.Notification(method, params)
    }

    given responseEncoder: Encoder[Response] = Encoder.instance {
      case Response.Success(result, id) =>
        Json.obj(
          "jsonrpc" -> Json.fromString(Version),
          "result" -> result,
          "id" -> id.asJson
        )

      case Response.Failure(error, id) =>
        Json.obj(
          "jsonrpc" -> Json.fromString(Version),
          "error" -> error.asJson,
          "id" -> id.asJson
        )

      case Response.BatchResponse(responses) =>
        Json.arr(responses.map(_.asJson(using responseEncoder))*)
    }

    given responseDecoder: Decoder[Response] = Decoder.instance { cursor =>
      if cursor.focus.exists(_.isArray) then
        cursor.as[List[Json]].flatMap { jsons =>
          val decodedResponses =
            jsons.map(_.as[Response](using responseDecoder))
          val errors = decodedResponses.collect { case Left(e) => e }
          if errors.nonEmpty then Left(errors.head)
          else
            Right(Response.BatchResponse(decodedResponses.collect {
              case Right(r) => r
            }))
        }
      else
        for
          jsonrpc <- cursor.get[String]("jsonrpc")
          _ <-
            if jsonrpc == Version then Right(())
            else
              Left(
                DecodingFailure(
                  s"Expected jsonrpc version $Version",
                  cursor.history
                )
              )
          id <- cursor.get[RequestId]("id")
          resultCursor = cursor.downField("result")
          errorCursor = cursor.downField("error")
          hasResult = resultCursor.focus.isDefined
          hasError = errorCursor.focus.isDefined
          _ <-
            if hasResult && hasError then
              Left(
                DecodingFailure(
                  "Cannot have both result and error",
                  cursor.history
                )
              )
            else Right(())
          _ <-
            if !hasResult && !hasError then
              Left(
                DecodingFailure(
                  "Must have either result or error",
                  cursor.history
                )
              )
            else Right(())
        yield
          if hasResult then Response.Success(resultCursor.focus.get, id)
          else
            Response.Failure(
              errorCursor
                .as[Error]
                .getOrElse(Error(ErrorCode.InternalError, "Unknown error")),
              id
            )
    }
