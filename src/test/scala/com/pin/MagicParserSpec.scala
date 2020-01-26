package com.pin

import org.scalatest.{Matchers, WordSpec}
import implicits.defaultDelimiters

class MagicParserSpec extends WordSpec with Matchers {

  "Magic parser" should {
    "parse simple row stream" in {

      val inputStream: Stream[String] =
        "aaa, bbb" #::
          """ccc, ddd, "eee, fff" """ #::
          "ccc" #::
          Stream.empty

      MagicParser.parse(inputStream).toList shouldBe
        List("aaa", " bbb") ::
          List("ccc", " ddd", " \"eee, fff\" ") ::
          List("ccc") ::
          Nil
    }

    "parse with stream with quoted eol" in {
      val inputStream: Stream[String] =
        "\"aaa" #::
          "bbb\"" #::
          Stream.empty

      MagicParser.parse(inputStream).toList shouldBe
        List("\"aaa\nbbb\"") :: Nil
    }

    "parse with headers" in {
      val inputStream =
        "col1,col2,col3" #::
          "r11,r12,r13" #::
          "r21,r22,r23,r24" #:: // r24 should be discarded
          "," #:: // missing cell should be added
          Stream.empty

      MagicParser.parseWithHeader(inputStream).toList shouldBe
        Seq(("col1", "r11"), ("col2", "r12"), ("col3", "r13")) ::
          Seq(("col1", "r21"), ("col2", "r22"), ("col3", "r23")) ::
          Seq(("col1", ""), ("col2", ""), ("col3", "")) ::
          Nil
    }
  }
}
