package dev.capslock.mcpscala.macros

import scala.quoted.*
import cats.effect.IO
import scala.compiletime.summonInline
import io.circe.{Json, Encoder, Decoder}
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
  )(using quotes: Quotes): Expr[HandlerFunc] = {
    import quotes.reflect.*

    // PartialFunctionからパラメータ情報を抽出
    val params = extractPartialFunctionParams(func)

    params match {
      case Some((paramTypes, paramNames)) =>
        // TODO: パラメータ情報を用いてハンドラーを生成
        generateByNameHandler(func, paramTypes, paramNames, encoderExpr)
      case None =>
        report.errorAndAbort(
          "Invalid pattern in partial function. Expected case (param1: Type1, param2: Type2, ...) => ..."
        )
        '{ ??? } // コンパイルエラーを避けるためのダミー式
    }
  }

  // JSONRPCハンドラを生成する補助関数
  // 最も基本的なハンドラーを生成する関数
  private def generateByNameHandler[Res: Type](using quotes: Quotes)(
      func: Expr[PartialFunction[Any, IO[Res]]],
      paramTypes: List[quotes.reflect.TypeRepr],
      paramNames: List[String],
      encoderExpr: Expr[Encoder[Res]]
  ): Expr[HandlerFunc] = {
    import quotes.reflect.*
    
    val paramPairs = paramNames.zip(paramTypes)
    val paramNamesExpr = Expr(paramNames)
    
    '{
      (params: JsonRpc.Params) => {
        params match {
          case JsonRpc.Params.ByName(values) => {
            import io.circe.syntax.*
            
            type Expected = (String, Int)
            
            val decoderString = summon[Decoder[String]]
            val decoderInt = summon[Decoder[Int]]
            
            val params: Option[Expected] = {
              try {
                val aResult = values.get("a").flatMap(_.as[String](decoderString).toOption)
                val bResult = values.get("b").flatMap(_.as[Int](decoderInt).toOption)
                
                (aResult, bResult) match {
                  case (Some(a), Some(b)) => Some((a, b))
                  case _ => None
                }
              } catch {
                case _: Exception => None
              }
            }
            
            params match {
              case None =>
                IO.pure(Left(JsonRpc.Error(
                  JsonRpc.ErrorCode.InvalidParams,
                  "Parameters do not match any pattern"
                )))
              case Some(p) =>
                val handlerFunc = ${func}
                if (handlerFunc.isDefinedAt(p)) {
                  handlerFunc(p).map { res =>
                    val json = res.asJson(using ${encoderExpr})
                    Right(json)
                  }
                } else {
                  IO.pure(Left(JsonRpc.Error(
                    JsonRpc.ErrorCode.InternalError,
                    "Error applying function: parameters do not match function definition"
                  )))
                }
            }
          }
          case _ =>
            IO.pure(Left(JsonRpc.Error(
              JsonRpc.ErrorCode.InvalidParams,
              "Only named parameters are supported"
            )))
        }
      }
    }
  }

  // PartialFunctionからパラメータの情報を抽出する
  private def extractPartialFunctionParams[Res: Type, F[_]: Type](
      func: Expr[PartialFunction[Any, F[Res]]]
  )(using
      quotes: Quotes
  ): Option[(List[quotes.reflect.TypeRepr], List[String])] = {
    import quotes.reflect.*

    // PartialFunctionの実装を取得
    func.asTerm match {
      case Inlined(
            _,
            _,
            Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _)
          ) =>
        // 最初のcase節を取得
        cases.headOption.flatMap { caseClause =>
          caseClause match {
            // タプルパターンの場合
            case CaseDef(TypedOrTest(t: Unapply, _), _ , _) =>
              val paramTypes = t.patterns.map {
                case Bind(a, TypedOrTest(_,b)) => b.tpe
              }
              val paramNames = t.patterns.map {
                case Bind(a, TypedOrTest(_,b)) => a
              }
              Some((paramTypes, paramNames))

            // 単一パラメータの場合
            case CaseDef(Typed(Bind(patternName, _), patternType), _, _) =>
              val tpeRepr = patternType.tpe
              Some((List(tpeRepr), List(patternName)))

            case _ =>
              report.errorAndAbort(s"Unsupported pattern in case clause")
              None
          }
        }
      case _ =>
        report.errorAndAbort(
          s"Could not extract partial function implementation"
        )
        None
    }
  }
}
