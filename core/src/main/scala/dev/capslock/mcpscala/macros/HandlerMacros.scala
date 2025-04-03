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
    report.info(s"Extracting case patterns from: ${funcExpr.show}")
    
    // デバッグ用に詳細なAST構造を出力
    report.info(s"AST structure: ${funcExpr.asTerm.getClass.getName}")
    
    funcExpr.asTerm match {
      // Match式の場合
      case Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _) =>
        report.info("Matched Block(List(DefDef(_,_,_, Some(Match(_,cases)))),_)")
        cases
      // 直接Match式の場合
      case Match(_, cases) =>
        report.info("Matched Match(_,cases)")
        cases
      // Lambda式の場合（x => x match { case ... }）
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _)) =>
        report.info("Matched Inlined(_,_, Block(...))")
        cases
      // その他の場合（詳細をログに出力）
      case other =>
        report.info(s"Unmatched pattern in extractCasePatterns. Full AST: ${other.show}")
        Nil
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
 
  inline def byPositionHandler[Res](inline func: PartialFunction[Any, IO[Res]])(using enc: Encoder[Res]): HandlerFunc =
    ${ byPositionHandlerImpl('func, 'enc) }
    
  private def byPositionHandlerImpl[Res: Type](func: Expr[PartialFunction[Any, IO[Res]]], encoderExpr: Expr[Encoder[Res]])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*
    
    // エンコーダをキャプチャした変数として提供する
    '{
      val encoderCaptured = $encoderExpr
      ${byPositionHandlerImplWithCapture[Res](func, 'encoderCaptured)}
    }
  }

  // エンコーダをキャプチャした変数を使用するためのヘルパー関数
  private def byPositionHandlerImplWithCapture[Res: Type](func: Expr[PartialFunction[Any, IO[Res]]], encoderExpr: Expr[Encoder[Res]])(using q: Quotes): Expr[HandlerFunc] = {
    import q.reflect.*

    def makeErrorExpr(code: Expr[JsonRpc.ErrorCode], msg: Expr[String]): Expr[JsonRpc.Error] =
      '{ JsonRpc.Error($code, $msg) }

    // PartialFunctionからケースパターンを抽出
    val casePatterns = extractCasePatterns(func)
    
    // caseパターンが見つからなかった場合はエラー
    if (casePatterns.isEmpty) {
      report.errorAndAbort("byPositionHandler requires a partial function with case patterns")
    }
    
    report.info(s"Found ${casePatterns.length} case patterns in byPositionHandler")
    
    // 結果を組み立てる
    '{
      (params: JsonRpc.Params) =>
        params match {
          case JsonRpc.Params.ByPosition(values) =>
            // 値をリストからAnyに変換
            val argsAny: Any =
              if (values.isEmpty) {
                // 引数がない場合
                ()
              } else if (values.size == 1) {
                // 単一引数の場合
                values.head
              } else {
                // 複数引数の場合はpositionに基づいてタプルに変換
                values.size match {
                  case 2 => (values(0), values(1))
                  case 3 => (values(0), values(1), values(2))
                  case _ => values // その他の場合はリストをそのまま渡す
                }
              }
            
            // PartialFunctionを適用
            val pfunc = $func
            
            // PartialFunctionが適用可能か確認
            if (pfunc.isDefinedAt(argsAny)) {
              try {
                // 関数を実行
                pfunc(argsAny).flatMap { result =>
                  // キャプチャしたエンコーダを使用
                  IO.pure(Right($encoderExpr(result)))
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
          
          case JsonRpc.Params.ByName(_) =>
            IO.pure(Left(${makeErrorExpr('{JsonRpc.ErrorCode.InvalidParams}, '{s"Expected positional parameters"})}))
        }
    }
  }

  // 古い実装は削除

  // 未使用のヘルパーメソッドは削除
}
