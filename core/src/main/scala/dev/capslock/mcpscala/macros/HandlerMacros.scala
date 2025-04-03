package dev.capslock.mcpscala.macros

import scala.quoted.*
import cats.effect.IO
import scala.compiletime.summonInline
import io.circe.{Json, Encoder, Decoder}
import dev.capslock.mcpscala.JsonRpc

object HandlerMacros {

  type HandlerFunc = JsonRpc.Params => IO[Either[JsonRpc.Error, Json]]

  // --- byNameHandler マクロ ---
  inline def byNameHandler[Res](inline func: PartialFunction[Any, IO[Res]])(using enc: Encoder[Res]): HandlerFunc =
    ${ byNameHandlerImpl('func, 'enc) }

  private def byNameHandlerImpl[Res: Type](func: Expr[PartialFunction[Any, IO[Res]]], encoderExpr: Expr[Encoder[Res]])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*

    // 型情報を取得
    val funcType = func.asTerm.tpe.widen
    
    // PartialFunctionのケースパターンを抽出する (match-case部分の分析)
    val casePatterns = extractCasePatterns(func)
    
    // caseパターンが見つからなかった場合はエラー
    if (casePatterns.isEmpty) {
      report.errorAndAbort("byNameHandler requires a partial function with case patterns")
    }
    
    // 結果を組み立てる
    '{ 
      // ハンドラ関数を定義する際、エンコーダはキャプチャした変数として渡す
      val encoderCaptured = $encoderExpr
      
      (params: JsonRpc.Params) =>
        params match {
          case JsonRpc.Params.ByName(values) =>
            // 値をMapからタプルに変換
            val argsAny: Any = extractMapValues(values)
            
            // PartialFunctionを適用
            val pfunc = $func
            
            // PartialFunctionが適用可能か確認
            if (pfunc.isDefinedAt(argsAny)) {
              try {
                // 関数を実行
                pfunc(argsAny).flatMap { result =>
                  // キャプチャしたエンコーダを使用
                  IO.pure(Right(encoderCaptured(result)))
                }.handleErrorWith { error =>
                  IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InternalError, s"Runtime error: ${error.getMessage}")))
                }
              } catch {
                case e: Throwable =>
                  IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InternalError, s"Error applying function: ${e.getMessage}")))
              }
            } else {
              IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InvalidParams, "Parameters do not match any pattern")))
            }
          
          case JsonRpc.Params.ByPosition(_) =>
            IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InvalidParams, "Expected named parameters")))
        }
    }
  }
  
  private def extractCasePatterns(using q: Quotes)(funcExpr: Expr[PartialFunction[Any, Any]]): List[q.reflect.CaseDef] = {
    import q.reflect.*
    
    // PartialFunctionのAST解析
    funcExpr.asTerm match {
      // Match式の場合
      case Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _) => cases
      // 直接Match式の場合
      case Match(_, cases) => cases
      // その他の場合（lambda式など）
      case _ => Nil
    }
  }
  
  // 実行時にマップから値を取り出すヘルパー関数
  private def extractMapValues(map: Map[String, Json]): Any = {
    // マップのキーの数によって、適切なタプルまたは単一値を返す
    map.size match {
      case 1 => 
        // 1つの値の場合は単純に最初の値を返す
        map.values.head
      case 2 =>
        // タプル化が必要な場合は、キーでソートして順番に取り出す
        val sortedValues = map.toList.sortBy(_._1).map(_._2)
        (sortedValues(0), sortedValues(1))
      case 3 =>
        val sortedValues = map.toList.sortBy(_._1).map(_._2)
        (sortedValues(0), sortedValues(1), sortedValues(2))
      case _ =>
        // その他のケースはマップをそのまま返す
        map
    }
  }

  // --- byPositionHandler マクロ ---
 
  inline def byPositionHandler[F](inline func: F): HandlerFunc =
    ${ byPositionHandlerImpl('func) }
    
  private def byPositionHandlerImpl[F: Type](func: Expr[F])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*
    
    // 1. 関数型からIO[Res]のRes部分を抽出
    val fTypeRepr = TypeRepr.of[F]
    
    def extractIOResultType(tpe: TypeRepr): TypeRepr = {
      tpe.dealias match {
        case AppliedType(base, args) if base =:= TypeRepr.of[IO] =>
          // IO[Res]の場合、型パラメータ（Res）を返す
          args.head
        case AppliedType(_, resultType :: _) =>
          // 関数型の場合、戻り値型を再帰的に探索
          extractIOResultType(resultType)
        case _ =>
          report.errorAndAbort(s"戻り値型からIO[Res]が見つかりませんでした: ${tpe.show}")
      }
    }
    
    val resultTypeRepr = extractIOResultType(fTypeRepr)
    
    // 2. 抽出した型に対応するEncoderを取得し、既存のマクロ実装を呼び出す
    resultTypeRepr.asType match {
      case '[res] =>
        // マクロ展開されたコード内で解決するように変更
        // このようにして実行時に具体的なエンコーダを解決する
        '{
          val encoderCaptured = summonInline[Encoder[res]]
          ${byPositionHandlerImplWithCapture[F, res](func, 'encoderCaptured)}
        }
      case _ =>
        report.errorAndAbort(s"型情報を取得できませんでした: ${resultTypeRepr.show}")
    }
  }

  // エンコーダをキャプチャした変数を使用するためのヘルパー関数
  private def byPositionHandlerImplWithCapture[F: Type, Res: Type](func: Expr[F], encoderExpr: Expr[Encoder[Res]])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*

    def makeErrorExpr(code: Expr[JsonRpc.ErrorCode], msg: Expr[String]): Expr[JsonRpc.Error] =
      '{ JsonRpc.Error($code, $msg) }

    // 1. Extract parameter types from TypeRepr.of[F]
    val fTypeRepr = TypeRepr.of[F]
    val paramTypes: List[TypeRepr] = fTypeRepr.dealias match {
      // Match MethodType or LambdaType (PolyType might need handling too)
      case MethodType(_, paramTpes, _) => paramTpes
      case mt @ AppliedType(base, args) if mt <:< TypeRepr.of[Function] =>
        // For FunctionN[T1, ..., TN, R], args are T1, ..., TN, R
        // We need T1, ..., TN
        if (args.nonEmpty) args.init else Nil
      // Handle cases like () => IO[Res] which might not be MethodType directly
      case AppliedType(ioType, _) if ioType =:= TypeRepr.of[IO] =>
        Nil // Zero parameters case
      case other =>
        report.errorAndAbort(s"byPositionHandler expects a function type, but got: ${other.show}", func)
    }

    // We already have Type[Res] from the macro definition
    val resultType = TypeRepr.of[Res]
    report.info(s"byPositionHandler: Params: ${paramTypes.map(_.show).mkString(", ")}. Result: ${resultType.show}")

    // 2. Generate decoding logic Expr
    val decodeFunctionExpr = generatePositionalDecodingCodeExpr(paramTypes)

    // 3. Generate apply logic Expr
    val applyFunctionExpr = generateApplyCode[Res](func, paramTypes)

    // 4. Combine in the final HandlerFunc Expr
    '{ 
       (params: JsonRpc.Params) =>
         params match {
           case JsonRpc.Params.ByPosition(paramList) =>
             ${decodeFunctionExpr}(paramList).fold[IO[Either[JsonRpc.Error, Json]]](
               decodeError => IO.pure(Left(decodeError)),
               decodedArgs => {
                 val theApplyFunction: Seq[Any] => IO[Res] = ${applyFunctionExpr}
                 theApplyFunction(decodedArgs).flatMap { result =>
                   // キャプチャしたエンコーダを使用
                   IO.pure(Right($encoderExpr(result)))
                 }.handleErrorWith { throwable =>
                   IO.pure(Left(JsonRpc.Error(JsonRpc.ErrorCode.InternalError, "Runtime error: " + throwable.getMessage)))
                 }
               }
             )
           case JsonRpc.Params.ByName(_) =>
             IO.pure(Left(${makeErrorExpr('{JsonRpc.ErrorCode.InvalidParams}, '{s"Expected positional parameters"})}))
         }
     }
  }

  private def byPositionHandlerImpl[F: Type, Res: Type](func: Expr[F], encoderExpr: Expr[Encoder[Res]])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*

    // エンコーダをキャプチャした変数として提供する
    '{ 
      val encoderCaptured = $encoderExpr
      ${byPositionHandlerImplWithCapture[F, Res](func, 'encoderCaptured)}
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
                     summonInline[Decoder[t]] match {
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
