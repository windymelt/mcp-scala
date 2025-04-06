package dev.capslock.mcpscala
package transport

import cats.effect._
import cats.syntax.all._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import Handler.MethodHandlers

object Server {
  import JsonRpc._
  import JsonRpc.Codec.given

  def jsonRpcService(methodHandlers: MethodHandlers[IO] = Map.empty) =
    HttpRoutes.of[IO] {
      case GET -> Root =>
        Ok("MCP Scala.")
      case req @ POST -> Root / "rpc" =>
        req.as[String].flatMap { body =>
          IO.println(body) >> handleJsonRpcRequest(methodHandlers)(body)
            .flatMap { response =>
              Ok(response)
                .map(
                  _.withContentType(`Content-Type`(MediaType.application.json))
                )
            }
        }
    }

  def handleJsonRpcRequest(
      methodHandlers: MethodHandlers[IO]
  )(body: String): IO[String] = {
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
