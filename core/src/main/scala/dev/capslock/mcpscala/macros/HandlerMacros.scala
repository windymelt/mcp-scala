package dev.capslock.mcpscala.macros

import scala.quoted.*
import cats.effect.IO
import scala.compiletime.summonInline
import io.circe.{Json, Encoder}
import dev.capslock.mcpscala.JsonRpc

object HandlerMacros {

  type HandlerFunc = JsonRpc.Params => IO[Either[JsonRpc.Error, Json]]

  // --- byNameHandler マクロ ---
  inline def byNameHandler[Res](inline func: PartialFunction[Any, IO[Res]])(
      using enc: Encoder[Res]
  ): HandlerFunc =
    ${ byNameHandlerImpl('func, 'enc) }

  private def byNameHandlerImpl[Res: Type](
      func: Expr[PartialFunction[Any, IO[Res]]],
      encoderExpr: Expr[Encoder[Res]]
  )(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*
    ???
  }
}
