package dev.capslock.mcpscala.macros

import scala.quoted.*
import cats.effect.IO
import scala.compiletime.summonInline
import io.circe.{Json, Encoder, Decoder, DecodingFailure}
import dev.capslock.mcpscala.JsonRpc // Assuming in dev.capslock.mcpscala package

object HandlerMacros {

  type HandlerFunc = JsonRpc.Params => IO[Either[JsonRpc.Error, Json]]

  // --- byNameHandler Macro (Placeholder) ---
  inline def byNameHandler[Res](inline func: PartialFunction[Any, IO[Res]]): HandlerFunc =
    ${ byNameHandlerImpl('func) }

  private def byNameHandlerImpl[Res: Type](func: Expr[PartialFunction[Any, IO[Res]]])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*
    report.warning("byNameHandler not implemented yet")
    '{ (params: JsonRpc.Params) => IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InternalError, "Not implemented"))) }
  }

  // --- byPositionHandler Macro ---
  inline def byPositionHandler[Params, Res](inline func: Function[Params, IO[Res]])(using io.circe.Encoder[Res]): HandlerFunc =
    ${ byPositionHandlerImpl[Params, Res]('func) }

  private def byPositionHandlerImpl[Params, Res: Type](func: Expr[Function[Params, IO[Res]]])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*

    def makeErrorExpr(code: Expr[JsonRpc.ErrorCode], msg: Expr[String]): Expr[JsonRpc.Error] =
      '{ JsonRpc.Error($code, $msg) }

    // Extract param types (bodyTerm is not needed for Apply)
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

    val resultType = TypeRepr.of[Res]
    report.info(s"byPositionHandler: Params: ${paramTypes.map(_.show).mkString(", ")}. Result: ${resultType.show}")

    val decodeFunctionExpr = generatePositionalDecodingCodeExpr(paramTypes)
    // Pass the original function Expr 'func' to generateApplyCode
    val applyFunctionExpr = generateApplyCode[Params, Res](func, paramTypes)

    '{ (params: JsonRpc.Params) =>
        params match {
          case JsonRpc.Params.ByPosition(paramList) =>
            ${decodeFunctionExpr}(paramList).fold[IO[Either[JsonRpc.Error, Json]]](
              decodeError => IO.pure(Left(decodeError)),
              decodedArgs => { // Seq[Any] at runtime
                // Execute apply logic at runtime
                ${applyFunctionExpr}(decodedArgs).flatMap { result => // result is Res
                  summonInline[Encoder[Res]] match {
                    case encoder => IO.pure(Right(encoder(result)))
                  }
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
                     val idxTerm = ${Expr(index)} // Store index in a runtime val
                     val jsonValue = paramList(idxTerm)
                     summonInline[Decoder[t]] match {
                       case decoder => decoder.decodeJson(jsonValue) match {
                         case Right(value) => Right(value.asInstanceOf[Any])
                         // Use idxTerm in the error message string
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
   private def generateApplyCode[Params, Res: Type](using q: Quotes)(funcExpr: Expr[Function[Params, cats.effect.IO[Res]]], paramTypes: List[q.reflect.TypeRepr]): Expr[Seq[Any] => IO[Res]] = {
       import q.reflect.*

       // Generate code that takes Seq[Any] and applies the original function Expr
       '{ (argsSeq: Seq[Any]) =>
           // This code runs at runtime
           ${
               // This code runs during macro expansion to generate the runtime Apply term

               // Build the list of argument Terms with casts from argsSeq Expr
               val argTerms: List[Term] = paramTypes.zipWithIndex.map { case (tpe, i) =>
                   tpe.asType match {
                       // Generate '{ argsSeq(i).asInstanceOf[t] }.asTerm
                       case '[t] => '{ argsSeq(${Expr(i)}).asInstanceOf[t] }.asTerm
                       case _ => report.errorAndAbort(s"Cannot get Type[T] for param $i: ${tpe.show}"); ???
                   }
               }

               // Construct the Apply term: funcExpr.asTerm(arg1, arg2, ...)
               // funcExpr.asTerm should represent the lambda function itself
               val funcTerm = funcExpr.asTerm
               // Apply the function Term to the argument Terms
               val applyTerm = Apply(funcTerm, argTerms)

               // The result should be IO[Res]
               applyTerm.asExprOf[IO[Res]]
           }
       }
   }

}
