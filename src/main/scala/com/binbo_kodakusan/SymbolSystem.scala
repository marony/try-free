package com.binbo_kodakusan

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
