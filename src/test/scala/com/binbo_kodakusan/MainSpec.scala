package com.binbo_kodakusan

import org.scalatest._

class MainSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Hello.greeting shouldEqual "hello"
  }
}
