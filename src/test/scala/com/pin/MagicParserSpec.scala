package com.pin

import org.scalatest.{Matchers, WordSpec}

class MagicParserSpec extends WordSpec with Matchers {

  "Magic parser" should {
    "parse simple row stream" in {
      val inputStream: Stream[String] =
        "aaa, bbb" #::
          """ccc, ddd, "eee, fff" """ #::
          "ccc" #::
          Stream.empty

      MagicParser.parsed(inputStream).toList shouldBe
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

      MagicParser.parsed(inputStream).toList shouldBe
        List("\"aaa\nbbb\"") :: Nil
    }
  }
}
