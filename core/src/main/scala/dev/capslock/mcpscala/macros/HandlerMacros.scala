package dev.capslock.mcpscala.macros

import scala.quoted.*
import cats.effect.IO
import scala.compiletime.summonInline
import io.circe.{Json, Encoder, Decoder, DecodingFailure}
import dev.capslock.mcpscala.JsonRpc // Assuming in dev.capslock.mcpscala package
// Import the runtime helper - no longer needed for this approach
// import dev.capslock.mcpscala.macros.HandlerMacrosRuntime.*

object HandlerMacros {

  type HandlerFunc = JsonRpc.Params => IO[Either[JsonRpc.Error, Json]]

  // --- byNameHandler Macro (Placeholder) ---
  // TODO: Apply the same Encoder logic here later
  inline def byNameHandler[Res](inline func: PartialFunction[Any, IO[Res]]): HandlerFunc =
    ${ byNameHandlerImpl('func) }

  private def byNameHandlerImpl[Res: Type](func: Expr[PartialFunction[Any, IO[Res]]])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*
    report.warning("byNameHandler not implemented yet")
    '{ (params: JsonRpc.Params) => IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InternalError, "Not implemented"))) }
  }

  // --- byPositionHandler Macro ---
  // Use type parameter F and add 'using Encoder[Res]'
  inline def byPositionHandler[F, Res](inline func: F)(using enc: Encoder[Res]): HandlerFunc =
    // Pass the summoned Encoder Expr to the Impl method
    ${ byPositionHandlerImpl[F, Res]('func, 'enc) }

  // Implementation takes Expr[F] and Expr[Encoder[Res]]
  private def byPositionHandlerImpl[F: Type, Res: Type](func: Expr[F], encoderExpr: Expr[Encoder[Res]])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*

    def makeErrorExpr(code: Expr[JsonRpc.ErrorCode], msg: Expr[String]): Expr[JsonRpc.Error] =
      '{ JsonRpc.Error($code, $msg) }

    // 1. Extract param types
    val paramTypes: List[TypeRepr] = func.asTerm match {
      case Inlined(_, _, Block(List(DefDef(_, List(TermParamClause(params)), _, _)), Closure(_, _))) =>
        params.map(_.tpt.tpe)
      case Lambda(valDefs, _) =>
        valDefs.map(_.tpt.tpe)
      case Inlined(_, _, Block(Nil, expr)) => expr match {
         case Lambda(valDefs, _) => valDefs.map(_.tpt.tpe)
         case other => report.errorAndAbort(s"Expected lambda in block, got ${other.show}", func); ???
      }
      case other => report.errorAndAbort(s"Expected function literal, got ${other.show}", func); ???
    }

    val resultType = TypeRepr.of[Res] // For info only
    report.info(s"byPositionHandler: Params: ${paramTypes.map(_.show).mkString(", ")}. Result: ${resultType.show}")

    // 2. Generate decoding logic Expr
    val decodeFunctionExpr = generatePositionalDecodingCodeExpr(paramTypes)

    // 3. Generate apply logic Expr
    val applyFunctionExpr = generateApplyCode[Res](func, paramTypes)

    // 4. Combine in the final HandlerFunc Expr, using the passed encoderExpr
    '{ (params: JsonRpc.Params) =>
        params match {
          case JsonRpc.Params.ByPosition(paramList) =>
            ${decodeFunctionExpr}(paramList).fold[IO[Either[JsonRpc.Error, Json]]](
              decodeError => IO.pure(Left(decodeError)),
              decodedArgs => { // Seq[Any] at runtime
                val theApplyFunction: Seq[Any] => IO[Res] = ${applyFunctionExpr}
                theApplyFunction(decodedArgs).flatMap { result => // result is Res
                  // Use the encoderExpr passed from the macro call site
                  val encoder: Encoder[Res] = ${encoderExpr}
                  IO.pure(Right(encoder(result)))
                }.handleErrorWith { throwable =>
                  IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InternalError, "Runtime error: " + throwable.getMessage)))
                }
              }
            )
          case JsonRpc.Params.ByName(_) =>
            IO.pure(Left(${makeErrorExpr('{JsonRpc.ErrorCode.InvalidParams}, Expr("Expected positional parameters"))}))
        }
    }
  }

  // --- Helper to generate the decoding code Expr ---
  private def generatePositionalDecodingCodeExpr(using q: Quotes)(paramTypes: List[q.reflect.TypeRepr]): Expr[List[Json] => Either[JsonRpc.Error, Seq[Any]]] = {
     import q.reflect.*
     '{ (paramList: List[Json]) =>
         val requiredParams = ${Expr(paramTypes.length)}
         if (paramList.length != requiredParams) {
           Left(JsonRpc.Error(JsonRpc.ErrorCode.InvalidParams, s"Expected $requiredParams params, got ${paramList.length}"))
         } else {
           val resultsEithers: List[Either[JsonRpc.Error, Any]] = ${
             Expr.ofList(paramTypes.zipWithIndex.map { case (tpe, index) =>
               tpe.asType match {
                 case '[t] =>
                   '{
                     val idxTerm = ${Expr(index)}
                     val jsonValue = paramList(idxTerm)
                     summonInline[Decoder[t]] match { // summonInline Decoder inside generated code
                       case decoder => decoder.decodeJson(jsonValue) match {
                         case Right(value) => Right(value.asInstanceOf[Any])
                         case Left(df) => Left(JsonRpc.Error(JsonRpc.ErrorCode.InvalidParams, s"Decode fail index $idxTerm: ${df.getMessage}"))
                       }
                     }
                   }.asExprOf[Either[JsonRpc.Error, Any]]
                 case _ =>
                   val pos = Option(tpe.termSymbol).map(_.pos).flatten.getOrElse(Position.ofMacroExpansion)
                   report.errorAndAbort(s"Cannot get Type[T] for ${tpe.show}. Only static types supported.", pos)
                   '{ Left(JsonRpc.Error(JsonRpc.ErrorCode.InternalError, "Macro error")) }.asExprOf[Either[JsonRpc.Error, Any]]
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
   private def generateApplyCode[Res: Type](using q: Quotes)(funcExpr: Expr[Any], paramTypes: List[q.reflect.TypeRepr]): Expr[Seq[Any] => IO[Res]] = {
       import q.reflect.*
       '{ (argsSeq: Seq[Any]) =>
           ${
               val argTerms: List[Term] = paramTypes.zipWithIndex.map { case (tpe, i) =>
                   tpe.asType match {
                       case '[t] => '{ argsSeq(${Expr(i)}).asInstanceOf[t] }.asTerm
                       case _ => report.errorAndAbort(s"Cannot get Type[T] for param $i: ${tpe.show}"); ???
                   }
               }
               val funcTerm = funcExpr.asTerm
               val applyTerm = Apply(funcTerm, argTerms)
               applyTerm.asExprOf[IO[Res]]
           }
       }
   }

}
