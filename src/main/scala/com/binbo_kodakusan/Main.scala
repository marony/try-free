package com.binbo_kodakusan

object Main extends App {
  import scala.collection.mutable
  import com.binbo_kodakusan.Expression._

  def solve(s: String): Unit = {
    println(s)
    val parser = new MyParser
    val r = parser.parse(parser.expr, s)
    r match {
      case parser.Success(expression, r) => {
//        println("Success: " + expression + ", " + r)
        val ss = new SymbolSystem
        expression.visitExpression(ss)

        // アルゴリズム上同じ答えが出力されちゃうのでSetを使う
        val set = mutable.Set.empty[mutable.Map[String, Int]]
        val css = (0 until 10).toSeq.permutations
        for (cs <- css) {
          val vs = ss.getVariables(cs)
          if (expression.asInstanceOf[EqualOp].solve(vs) && !set.contains(vs)) {
            println(vs.toList.sortBy(_._1))
            set += vs
          }
        }
      }
      case parser.Failure(msg, r) => println("Failure: " + msg + ", " + r)
      case parser.Error(msg, r) => println("Success: " + msg + ", " + r)
    }
  }

  {
    val s = "TRY + TRY = FREE"
    solve(s)
  }
  {
    val s = "EYE + EYE = VIEW"
    solve(s)
  }
  {
    val s = "ここで - ねる = ねこ"
    solve(s)
  }
  {
    val s = "GIVE + ME = ITEM"
    solve(s)
  }
  {
    val s = "このねこ * だ = ねこだね"
    solve(s)
  }
  {
    val s = "WORD + WORD = CROSS"
    solve(s)
  }
  {
    val s = "MIGI + MIGI = RIGHT"
    solve(s)
  }
  {
    val s = "なみだめ + なみだめ = なかないで"
    solve(s)
  }
  {
    val s = "GOLF + GOLF = WEDGE"
    solve(s)
  }
  {
    val s = "CUTE + CUTE = TEENS"
    solve(s)
  }
  {
    val s = "かいいぬ * か = のらいぬか"
    solve(s)
  }
}
