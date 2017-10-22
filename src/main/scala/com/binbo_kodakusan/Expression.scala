package com.binbo_kodakusan

// 式
object Expression {
  import scala.collection.mutable
  // 式
  abstract trait Expr {
    def visitExpression(ss: SymbolSystem): Unit
    def eval(vs: mutable.Map[String, Int]): Int
  }
  // 二項演算子
  abstract class BinaryOp(lhs: Expr, rhs: Expr) extends Expr {
    override def visitExpression(ss: SymbolSystem): Unit = {
      lhs.visitExpression(ss)
      rhs.visitExpression(ss)
    }
  }
  // 等号
  sealed case class EqualOp(lhs: Expr, rhs: Expr)
    extends BinaryOp(lhs, rhs) {
    def eval(vs: mutable.Map[String, Int]): Int = ???
    def solve(vs: mutable.Map[String, Int]): Boolean = {
      val l = lhs.eval(vs)
      val r = rhs.eval(vs)
      l == r
    }
  }
  // 加算
  sealed case class PlusOp(lhs: Expr, rhs: Expr)
    extends BinaryOp(lhs, rhs) {
    override def eval(vs: mutable.Map[String, Int]): Int = {
      val l = lhs.eval(vs)
      val r = rhs.eval(vs)
      l + r
    }
  }
  // 減算
  sealed case class MinusOp(lhs: Expr, rhs: Expr)
    extends BinaryOp(lhs, rhs) {
    override def eval(vs: mutable.Map[String, Int]): Int = {
      val l = lhs.eval(vs)
      val r = rhs.eval(vs)
      l - r
    }
  }
  // 乗算
  sealed case class MulOp(lhs: Expr, rhs: Expr)
    extends BinaryOp(lhs, rhs) {
    override def eval(vs: mutable.Map[String, Int]): Int = {
      val l = lhs.eval(vs)
      val r = rhs.eval(vs)
      l * r
    }
  }
  // 変数
  sealed case class Var(sym: String) extends Expr {
    override def visitExpression(ss: SymbolSystem): Unit = {
      ss.setSym(sym)
    }
    override def eval(vs: mutable.Map[String, Int]): Int = vs(sym)
  }
  // 整数
  sealed case class Num(n: Int) extends Expr {
    override def visitExpression(ss: SymbolSystem): Unit = {}
    override def eval(vs: mutable.Map[String, Int]): Int = n
  }
}
