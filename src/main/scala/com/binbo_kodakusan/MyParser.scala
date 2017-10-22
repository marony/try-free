package com.binbo_kodakusan

import scala.util.parsing.combinator._

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
