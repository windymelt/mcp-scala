package dev.capslock.mcpscala.macros

import scala.quoted.*
import cats.effect.IO
import scala.compiletime.summonInline
import io.circe.{Json, Encoder, Decoder, DecodingFailure}
import dev.capslock.mcpscala.JsonRpc // Assuming in dev.capslock.mcpscala package

object HandlerMacros {
  type HandlerFunc = JsonRpc.Params => IO[Either[JsonRpc.Error, Json]]

  // --- byNameHandler Macro (Placeholder) ---
  inline def byNameHandler[Res](
      inline func: PartialFunction[Any, IO[Res]]
  ): HandlerFunc =
    ${ byNameHandlerImpl('func) }

  private def byNameHandlerImpl[Res: Type](
      func: Expr[PartialFunction[Any, IO[Res]]]
  )(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*
    report.warning("byNameHandler not implemented yet")
    '{ (params: JsonRpc.Params) =>
      IO.pure(
        Left(JsonRpc.Error(JsonRpc.ErrorCode.InternalError, "Not implemented"))
      )
    }
  }

  // --- byPositionHandler Macro ---
  inline def byPositionHandler[Res](inline func: Any): HandlerFunc =
    ${ byPositionHandlerImpl[Res]('func) }

  private def byPositionHandlerImpl[Res: Type](
      func: Expr[Any]
  )(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*

    def makeErrorExpr(
        code: Expr[JsonRpc.ErrorCode],
        msg: Expr[String]
    ): Expr[JsonRpc.Error] =
      '{ JsonRpc.Error($code, $msg) }

    // Extract param types and body term
    val (paramTypes: List[TypeRepr], bodyTerm: Term) = func.asTerm match {
      case Inlined(
            _,
            _,
            Block(
              List(DefDef(_, List(TermParamClause(params)), _, Some(rhs))),
              Closure(_, Some(lambdaRef))
            )
          ) =>
        // Use the lambdaRef (Ident) as the function to Apply if possible
        (params.map(_.tpt.tpe), lambdaRef)
      case Inlined(
            _,
            _,
            Block(
              List(DefDef(_, List(TermParamClause(params)), _, Some(rhs))),
              _
            )
          ) =>
        // Fallback if Closure doesn't provide lambdaRef
        (params.map(_.tpt.tpe), rhs)
      case Lambda(valDefs, rhs) =>
        (valDefs.map(_.tpt.tpe), rhs) // bodyTerm is the lambda body itself
      case Inlined(_, _, Block(Nil, expr)) =>
        expr match {
          case Lambda(valDefs, rhs) => (valDefs.map(_.tpt.tpe), rhs)
          case other =>
            report.errorAndAbort(
              s"Expected lambda in block, got ${other.show}",
              func
            ); ???
        }
      case other =>
        report.errorAndAbort(
          s"Expected function literal, got ${other.show}",
          func
        ); ???
    }

    val resultType = TypeRepr.of[Res]
    report.info(
      s"byPositionHandler: Params: ${paramTypes.map(_.show).mkString(", ")}. Result: ${resultType.show}"
    )

    val decodeFunctionExpr = generatePositionalDecodingCodeExpr(paramTypes)

    // Generate apply logic Expr: Seq[Any] => IO[Res]
    // Pass the original function Expr 'func' instead of bodyTerm
    val applyFunctionExpr = generateApplyCode[Res](func, paramTypes)

    // Combine in the final HandlerFunc Expr
    '{ (params: JsonRpc.Params) =>
      params match {
        case JsonRpc.Params.ByPosition(paramList) =>
          ${ decodeFunctionExpr }(paramList)
            .fold[IO[Either[JsonRpc.Error, Json]]](
              decodeError => IO.pure(Left(decodeError)),
              decodedArgs => { // Seq[Any] at runtime
                // Execute apply logic at runtime
                ${ applyFunctionExpr }(decodedArgs)
                  .flatMap { result => // result is Res
                    summonInline[Encoder[Res]] match {
                      case encoder => IO.pure(Right(encoder(result)))
                    }
                  }
                  .handleErrorWith { throwable =>
                    IO.pure(
                      Left(
                        JsonRpc.Error(
                          JsonRpc.ErrorCode.InternalError,
                          "Runtime error: " + throwable.getMessage
                        )
                      )
                    )
                  }
              }
            )
        case JsonRpc.Params.ByName(_) =>
          IO.pure(Left(${
            makeErrorExpr(
              '{ JsonRpc.ErrorCode.InvalidParams },
              Expr("Expected positional parameters")
            )
          }))
      }
    }
  }

  // --- Helper to generate the decoding code Expr ---
  private def generatePositionalDecodingCodeExpr(using q: Quotes)(
      paramTypes: List[quotes.reflect.TypeRepr]
  ): Expr[List[Json] => Either[JsonRpc.Error, Seq[Any]]] = {
    import q.reflect.*
    '{ (paramList: List[Json]) =>
      val requiredParams = ${ Expr(paramTypes.length) }
      if (paramList.length != requiredParams) {
        Left(
          JsonRpc.Error(
            JsonRpc.ErrorCode.InvalidParams,
            s"Expected $requiredParams params, got ${paramList.length}"
          )
        )
      } else {
        val resultsEithers: List[Either[JsonRpc.Error, Any]] = ${
          Expr.ofList(paramTypes.zipWithIndex.map { case (tpe, index) =>
            tpe.asType match {
              case '[t] =>
                '{
                  val jsonValue = paramList(${ Expr(index) })
                  summonInline[Decoder[t]] match {
                    case decoder =>
                      decoder.decodeJson(jsonValue) match {
                        case Right(value) => Right(value.asInstanceOf[Any])
                        case Left(df) =>
                          val indexExpr = ${ Expr(index) }
                          Left(
                            JsonRpc.Error(
                              JsonRpc.ErrorCode.InvalidParams,
                              s"Decode fail index ${indexExpr}: ${df.getMessage}"
                            )
                          )
                      }
                  }
                }.asExprOf[Either[JsonRpc.Error, Any]]
              case _ =>
                val pos = Option(tpe.termSymbol)
                  .map(_.pos)
                  .flatten
                  .getOrElse(Position.ofMacroExpansion)
                report.errorAndAbort(
                  s"Cannot get Type[T] for ${tpe.show}. Only static types supported.",
                  pos
                )
                '{
                  Left(
                    JsonRpc.Error(
                      JsonRpc.ErrorCode.InternalError,
                      "Macro error"
                    )
                  )
                }.asExprOf[Either[JsonRpc.Error, Any]]
            }
          })
        }
        resultsEithers.collectFirst { case Left(e) => Left(e) }.getOrElse {
          Right(resultsEithers.collect { case Right(v) => v })
        }
      }
    }
  }

  // --- Helper to generate the function application code ---
  // Generates Expr[Seq[Any] => IO[Res]]
  // Takes the original function Expr 'func'
  private def generateApplyCode[Res: Type](using q: Quotes)(
      funcExpr: Expr[Any],
      paramTypes: List[quotes.reflect.TypeRepr]
  ): Expr[Seq[Any] => IO[Res]] = {
    import q.reflect.*

    // Generate code that takes Seq[Any] and applies the original function Expr
    '{ (argsSeq: Seq[Any]) =>
      ${
        // Build the list of arguments with casts from argsSeq
        val argExprs: List[Expr[Any]] = paramTypes.zipWithIndex.map {
          case (tpe, i) =>
            tpe.asType match {
              // Generate '{ argsSeq(i).asInstanceOf[t] }
              case '[t] =>
                '{ argsSeq(${ Expr(i) }).asInstanceOf[t] }.asExprOf[Any]
              case _ =>
                report.errorAndAbort(
                  s"Cannot get Type[T] for param $i: ${tpe.show}"
                ); ???
            }
        }

        // Construct the Apply term: func(arg1, arg2, ...)
        // We apply the original function Expr directly
        Apply(funcExpr.asTerm, argExprs.map(_.asTerm)).asExprOf[IO[Res]]
      }
    }
  }

}
