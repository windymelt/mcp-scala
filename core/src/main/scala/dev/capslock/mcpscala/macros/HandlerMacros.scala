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
  // パラメータ型に対応するデコーダーの式を生成する関数
  private def generateDecoderExpr[T: Type](using quotes: Quotes): Expr[Decoder[T]] = {
    import quotes.reflect.*
    
    val tpe = TypeRepr.of[T]
    Expr.summon[Decoder[T]] match {
      case Some(decoderExpr) => decoderExpr
      case None => report.errorAndAbort(s"No Decoder instance found for type ${tpe.show}")
    }
  }
  
  // 単一パラメータの抽出コードを生成する関数
  private def generateParamExtraction[T: Type](
      using quotes: Quotes
  )(paramName: String, valueMapExpr: Expr[Map[String, Json]]): Expr[Option[T]] = {
    import quotes.reflect.*
    
    val decoderExpr = generateDecoderExpr[T]
    
    '{
      $valueMapExpr.get(${ Expr(paramName) }).flatMap(_.as[T](using $decoderExpr).toOption)
    }
  }
  
  // タプル型を生成する関数
  private def generateTupleType(using quotes: Quotes)(
      types: List[quotes.reflect.TypeRepr]
  ): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    
    types match {
      case Nil => TypeRepr.of[Unit]
      case tpe :: Nil => tpe  // 単一要素の場合はそのまま
      case _ =>
        // 複数要素の場合はタプル型を生成
        val tupleSymbol = Symbol.requiredClass(s"scala.Tuple${types.size}")
        val appliedType = TypeRepr.of(using tupleSymbol.typeRef.asType).appliedTo(types)
        appliedType
    }
  }
  
  // 複数パラメータの抽出結果を組み合わせる関数
  private def combineParamResults[T: Type](using quotes: Quotes)(
      results: List[Expr[Option[Any]]]
  ): Expr[Option[T]] = {
    import quotes.reflect.*
    
    results match {
      case Nil => '{ Some(().asInstanceOf[T]) }
      case result :: Nil => '{ $result.asInstanceOf[Option[T]] }
      case _ =>
        // Option同士を組み合わせてOption[Tuple]にする
        val combined = results.reduceLeft { (acc, next) =>
          '{
            for {
              a <- $acc
              b <- $next
            } yield (a, b)
          }
        }
        
        // ネストされたタプルを平坦化する処理
        if (results.size == 2) {
          // (a, b) の形式なのでそのまま返す
          '{ $combined.asInstanceOf[Option[T]] }
        } else {
          // ((a, b), c), ... となっているものを (a, b, c, ...) に変換
          '{
            $combined.map { tuple =>
              // 実行時にタプルを平坦化
              tuple.asInstanceOf[(Any, Any)] match {
                case (nestedTuple: Product, lastValue) =>
                  val elements = nestedTuple.productIterator.toSeq :+ lastValue
                  scala.runtime.Tuples.fromArray(elements.toArray.asInstanceOf[Array[Object]]).asInstanceOf[T]
              }
            }
          }
        }
    }
  }
  
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
            
            // TODO: 型のリストが分かればfoldRightしてTuple型を生成できる
            // (A, B, C) =:= A *: B *: C *: EmptyTuple
            type Expected = String *: Int *: EmptyTuple
            
            // TODO: パラメータが分かればDecoderをsummonする式を生成できる。
            val decoderString = summon[Decoder[String]]
            val decoderInt = summon[Decoder[Int]]
            
            val params: Option[Expected] = {
              try {
                // TODO: 名前と型が分かればfooResultを生成できる。ここを1つの式にできる
                // TODO: 
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
