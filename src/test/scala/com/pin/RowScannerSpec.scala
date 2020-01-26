package com.pin

import com.pin.implicits.delimiters
import org.scalatest.{Assertion, Matchers, Succeeded, WordSpec}

import scala.reflect.ClassTag


class RowScannerSpec extends WordSpec with Matchers {

  abstract class ParseEventMatcher[T <: ParserEvent: ClassTag] {
    def shouldMatch(pe: ParserEvent, expectations: Any*): Assertion = {
      pe shouldBe a[T]
      check(pe.asInstanceOf[T], expectations: _*)
    }

    def check(pe: T, expectations: Any*): Assertion
  }

  class WithContentMatcher[T <: WithContent: ClassTag] extends ParseEventMatcher[T] {
    override def check(pe: T, expectations: Any*): Assertion = {
      pe.content shouldBe expectations.head
    }
  }

  object ParseEventMatchers {
    implicit val runRowCompleteMatcher: ParseEventMatcher[RowComplete] = new ParseEventMatcher[RowComplete] {
      override def check(pe: RowComplete, expectations: Any*): Assertion = Succeeded
    }

    implicit val cellParcedMatcher: WithContentMatcher[CellParced] = new WithContentMatcher[CellParced]
    implicit val сellIncompleteMatcher: WithContentMatcher[CellIncomplete] = new WithContentMatcher[CellIncomplete]
    implicit val quotedMatcher: WithContentMatcher[Quoted] = new WithContentMatcher[Quoted]
    implicit val quotedIncompleteMatcher: WithContentMatcher[QuotedIncomplete] = new WithContentMatcher[QuotedIncomplete]
    implicit val escapedMatcher: WithContentMatcher[Escaped] = new WithContentMatcher[Escaped]
    implicit val quotedEscapedMatcher: WithContentMatcher[QuotedEscaped] = new WithContentMatcher[QuotedEscaped]


  }

  def checkEvent[T <: ParserEvent](expectations: Any*)(implicit matcher: ParseEventMatcher[T]): State[ParserEvent, Unit] = State { cs: ParserEvent =>
    val newCs = RowScanner.scan(cs)
    matcher.shouldMatch(newCs, expectations: _*)
    (newCs, Unit)
  }

  "Scanner" should {
    import ParseEventMatchers._

    "recognize unquoted cell" in {
      val input = " abc, def"

      val test: State[ParserEvent, Unit] = for {
        _ <- checkEvent[CellParced](" abc")
        _ <- checkEvent[CellIncomplete]("")
        _ <- checkEvent[CellParced](" def")
        _ <- checkEvent[RowComplete]()
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize quotation" in {
      val input = """ "abc" "def" ,"e,g,h" """

      val test = for {
        _ <- checkEvent[CellIncomplete](" ")
        _ <- checkEvent[Quoted](" ")
        _ <- checkEvent[CellIncomplete](""" "abc"""")
        _ <- checkEvent[CellIncomplete](""" "abc" """)
        _ <- checkEvent[Quoted](""" "abc" """)
        _ <- checkEvent[CellIncomplete](""" "abc" "def"""")
        _ <- checkEvent[CellParced](""" "abc" "def" """)
        _ <- checkEvent[CellIncomplete]("")
        _ <- checkEvent[Quoted]("")
        _ <- checkEvent[CellIncomplete](""""e,g,h"""")
        _ <- checkEvent[CellParced](""""e,g,h" """)
        _ <- checkEvent[RowComplete]()
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize mixed cell" in {
      val input = """ abc "def" """

      val test = for {
        _ <- checkEvent[CellIncomplete](" abc ")
        _ <- checkEvent[Quoted](" abc ")
        _ <- checkEvent[CellIncomplete](""" abc "def"""")
        _ <- checkEvent[CellParced](""" abc "def" """)
        _ <- checkEvent[RowComplete]()
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize unclosed quote" in {
      val input = "\""

      val test = for {
        _ <- checkEvent[Quoted]("")
        _ <- checkEvent[QuotedIncomplete]("\"")
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize empty cells" in {
      val input = ","

      val test = for {
        _ <- checkEvent[CellParced]("")
        _ <- checkEvent[CellIncomplete]("")
        _ <- checkEvent[CellParced]("")
        _ <- checkEvent[RowComplete]()
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize escaped symbol" in {
      val input = """a\""""

      val test = for {
        _ <- checkEvent[Escaped]("a")
        _ <- checkEvent[CellIncomplete]("a\"")
        _ <- checkEvent[CellParced]("a\"")
              } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize escaped symbol in the end of the line" in {
      val input = """a\"""

      val test = for {
        _ <- checkEvent[Escaped]("a")
        _ <- checkEvent[CellParced]("a\\")
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize escaped symbol within quote" in {
      val input = """ "\\""  """

      val test = for {
        _ <- checkEvent[CellIncomplete](" ")
        _ <- checkEvent[Quoted](" ")
        _ <- checkEvent[QuotedEscaped](" \"")
        _ <- checkEvent[Quoted](" \"\\")
        _ <- checkEvent[CellIncomplete](""" "\""""")
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "scan complete row" in {
      val input = """a,, "abc" "def" ,"e,g,h" ,"""
      val (scannedRow, carryOver) = RowScanner.scanRow(Nil, RowScanner.startLine(input))
      carryOver shouldBe None
      scannedRow.reverse shouldBe "a" :: "" :: """ "abc" "def" """ :: """"e,g,h" """ :: "" :: Nil
    }

    "scan row with unclosed quotation" in {
      val input = """a,, "abc" "def" ,"e,g,h"""
      val (scannedRow, carryOver) = RowScanner.scanRow(Nil, RowScanner.startLine(input))
      scannedRow shouldBe """ "abc" "def" """ :: "" :: "a" :: Nil
      carryOver shouldBe Some(QuotedIncomplete(input, 17, "\"e,g,h"))
    }

    "scan with alternative delimiters" in {
      val input = "'ccc:ddd' : 'eee',fff "
      val altDelimiters = new Delimiters {
        override val quote = '''
        override val cellSeparator = ':'
      }

      val (scannedRow, _) = RowScanner.scanRow(Nil, RowScanner.startLine(input))(altDelimiters)
      scannedRow.reverse shouldBe List("'ccc:ddd' ", " 'eee',fff ")
    }
  }
}
