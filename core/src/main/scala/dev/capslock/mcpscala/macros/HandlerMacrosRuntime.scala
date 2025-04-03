package dev.capslock.mcpscala.macros

import cats.effect.IO
import io.circe.{Json, Encoder, Decoder, DecodingFailure}
import dev.capslock.mcpscala.JsonRpc

object HandlerMacrosRuntime {

  // Runtime helper for positional handlers
  def decodeAndApplyPositional[Res](
      paramList: List[Json],
      decoders: List[Decoder[?]], // Decoders obtained by the macro
      func: Any, // The original function passed to the macro
      encoder: Encoder[Res] // Encoder obtained by the macro
  ): IO[Either[JsonRpc.Error, Json]] = {

    val requiredParams = decoders.length
    if (paramList.length != requiredParams) {
      IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InvalidParams, s"Expected $requiredParams params, got ${paramList.length}")))
    } else {
      // 1. Decode parameters using provided decoders
      val decodeResults: List[Either[DecodingFailure, Any]] =
        paramList.zip(decoders).zipWithIndex.map { case ((json, decoder), index) =>
          // Cast the decoder back to Decoder[T] - this is unsafe but necessary here
          // The macro ensures the order matches the function signature
          type T // Placeholder type
          val typedDecoder = decoder.asInstanceOf[Decoder[T]]
          typedDecoder.decodeJson(json).map(_.asInstanceOf[Any]) // Store as Any
            .left.map(df => DecodingFailure(s"Decode fail index $index: ${df.message}", df.history))
        }

      // 2. Check for decoding errors
      decodeResults.collectFirst { case Left(df) => df } match {
        case Some(firstError) =>
          IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InvalidParams, firstError.getMessage)))
        case None =>
          // All parameters decoded successfully
          val decodedArgs: Seq[Any] = decodeResults.collect { case Right(v) => v }

          // 3. Apply the function (This is the tricky part without macros)
          // We need to dynamically call 'func' with 'decodedArgs'
          // This typically requires Java reflection if 'func' is Any.
          val resultIO: IO[Res] = try {
            // --- Reflection approach (Example - might need refinement) ---
            val funcMethod = func.getClass.getMethod("apply", decodedArgs.map(_.getClass): _*) // Unsafe getClass
            // This assumes func is an instance of FunctionN and we can find the apply method.
            // Argument types might not match exactly due to erasure or boxing.
            // funcMethod.invoke(func, decodedArgs.map(_.asInstanceOf[Object]): _*) match {
            //   case io: IO[?] => io.asInstanceOf[IO[Res]] // Very unsafe cast
            //   case _ => IO.raiseError(new RuntimeException("Handler function did not return IO"))
            // }

            // --- Simpler (but likely incorrect) approach for Function1 ---
             if (decodedArgs.length == 1) {
               func.asInstanceOf[Any => IO[Res]](decodedArgs.head)
             } else if (decodedArgs.isEmpty) {
                func.asInstanceOf[() => IO[Res]]()
             }
             else {
                 // TODO: Handle other arities using reflection or a different strategy
                 IO.raiseError(new NotImplementedError(s"Runtime application for ${decodedArgs.length} args not implemented"))
             }
          } catch {
            case e: Throwable => IO.raiseError(new RuntimeException(s"Failed to apply handler function: ${e.getMessage}", e))
          }

          // 4. Encode the result and handle errors
          resultIO.flatMap { result =>
            IO.pure(Right(encoder(result)))
          }.handleErrorWith { throwable =>
            IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InternalError, "Runtime error: " + throwable.getMessage)))
          }
      }
    }
  }

  // TODO: Add decodeAndApplyByName helper later

}