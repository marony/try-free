package com.binbo_kodakusan

import scala.util.parsing.combinator._

class SymbolSystem {
  import scala.collection.mutable

  val variables = mutable.Set.empty[String]

  def setSym(sym: String): Unit = {
    if (!variables.contains(sym)) {
      variables += sym
    }
  }

  def getVariables(cs: Seq[Int]): mutable.Map[String, Int] = {
    val r = mutable.Map.empty[String, Int]
    val vs = variables.zip(cs)
    for (v <- vs) yield {
      r(v._1) = v._2
    }
    r
  }
}

object Expression {
  import scala.collection.mutable

  abstract trait Expr {
    def visitExpression(ss: SymbolSystem): Unit
    def eval(vs: mutable.Map[String, Int]): Int
  }

  abstract class BinaryOp(lhs: Expr, rhs: Expr) extends Expr {
    override def visitExpression(ss: SymbolSystem): Unit = {
      lhs.visitExpression(ss)
      rhs.visitExpression(ss)
    }
  }

  sealed case class EqualOp(lhs: Expr, rhs: Expr)
    extends BinaryOp(lhs, rhs) {
    def eval(vs: mutable.Map[String, Int]): Int = ???
    def solve(vs: mutable.Map[String, Int]): Boolean = {
      val l = lhs.eval(vs)
      val r = rhs.eval(vs)
      l == r
    }
  }

  sealed case class PlusOp(lhs: Expr, rhs: Expr)
    extends BinaryOp(lhs, rhs) {
    override def eval(vs: mutable.Map[String, Int]): Int = {
      val l = lhs.eval(vs)
      val r = rhs.eval(vs)
      l + r
    }
  }

  sealed case class MinusOp(lhs: Expr, rhs: Expr)
    extends BinaryOp(lhs, rhs) {
    override def eval(vs: mutable.Map[String, Int]): Int = {
      val l = lhs.eval(vs)
      val r = rhs.eval(vs)
      l - r
    }
  }

  sealed case class MulOp(lhs: Expr, rhs: Expr)
    extends BinaryOp(lhs, rhs) {
    override def eval(vs: mutable.Map[String, Int]): Int = {
      val l = lhs.eval(vs)
      val r = rhs.eval(vs)
      l * r
    }
  }

  sealed case class Var(sym: String) extends Expr {
    override def visitExpression(ss: SymbolSystem): Unit = {
      ss.setSym(sym)
    }
    override def eval(vs: mutable.Map[String, Int]): Int = vs(sym)
  }

  sealed case class Num(n: Int) extends Expr {
    override def visitExpression(ss: SymbolSystem): Unit = {}
    override def eval(vs: mutable.Map[String, Int]): Int = n
  }
}

class MyParser extends RegexParsers {
  import com.binbo_kodakusan.Expression._

  def getVar(s: String): Expr = {
    val len = s.length
    s.toSeq match {
      case Seq(l, r, t@_*) => PlusOp(MulOp(Var(l.toString), Num(Math.pow(10, len - 1).toInt)), getVar(r.toString ++ t))
      case _ => Var(s)
    }
  }

  case class OpTerm(op: String, term: Expr)

  def getVar(t: Expr, opt: List[OpTerm]): Expr = {
    def loop(opt: List[OpTerm], a: Expr): Expr = {
      opt match {
        case OpTerm(op, term) :: t =>
          op match {
            case "=" => loop(t, EqualOp(a, term))
            case "*" => loop(t, MulOp(a, term))
            case "+" => loop(t, PlusOp(a, term))
            case "-" => loop(t, MinusOp(a, term))
          }
        case Nil => a
      }
    }
    loop(opt, t)
  }

  def term: Parser[Expr] = """[^ 0ー9+=-]+""".r ^^ { getVar(_) }
  def opTerm: Parser[OpTerm] = """ *(\*|\+|-|=) *""".r ~ term ^^ { case o ~ t => OpTerm(o.trim, t) }
  def expr: Parser[Expr] = term ~ rep(opTerm) ^^ { case t ~ opt => getVar(t, opt) }
}

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
