package com.pin

import org.scalatest.{Matchers, WordSpec}
import implicits.delimiters

class RowScannerSpec extends WordSpec with Matchers {

  def assertEvent(expectation: ParserEvent): State[ParserEvent, Unit] = State { cs: ParserEvent =>
    val newCs = RowScanner.scan(cs)
    newCs shouldBe expectation
    (newCs, Unit)
  }

  "Scanner" should {
    "recognize unquoted cell" in {
      val input = " abc, def"

      val test: State[ParserEvent, Unit] = for {
        _ <- assertEvent(CellParced(input, 5, " abc"))
        _ <- assertEvent(CellIncomplete(input, 5, ""))
        _ <- assertEvent(CellParced(input, 10, " def"))
        _ <- assertEvent(RowComplete(input, 10))
      } yield ()

      test.run(RowScanner.startLine(input))

    }

    "recognize quotation" in {
      val input = """ "abc" "def" ,"e,g,h" """

      val test = for {
        _ <- assertEvent(CellIncomplete(input, position = 1, " "))
        _ <- assertEvent(Quoted(input, position = 1, " ", quotePosition = 1))
        _ <- assertEvent(CellIncomplete(input, position = 6, """ "abc""""))
        _ <- assertEvent(CellIncomplete(input, position = 7, """ "abc" """))
        _ <- assertEvent(Quoted(input, position = 7, """ "abc" """, quotePosition = 7))
        _ <- assertEvent(CellIncomplete(input, position = 12, """ "abc" "def""""))
        _ <- assertEvent(CellParced(input, position = 14, """ "abc" "def" """))
        _ <- assertEvent(CellIncomplete(input, position = 14, ""))
        _ <- assertEvent(Quoted(input, position = 14, "", quotePosition = 14))
        _ <- assertEvent(CellIncomplete(input, position = 21, """"e,g,h""""))
        _ <- assertEvent(CellParced(input, position = 23, """"e,g,h" """))
        _ <- assertEvent(RowComplete(input, 23))
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize mixed cell" in {
      val input = """ abc "def" """

      val test = for {
        _ <- assertEvent(CellIncomplete(input, 5, " abc "))
        _ <- assertEvent(Quoted(input, position = 5, " abc ", quotePosition = 5))
        _ <- assertEvent(CellIncomplete(input, 10, """ abc "def""""))
        _ <- assertEvent(CellParced(input, 12, """ abc "def" """))
        _ <- assertEvent(RowComplete(input, 12))
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize unclosed quote" in {
      val input = "\""

      val test = for {
        _ <- assertEvent(Quoted(input, position = 0, "", quotePosition = 0))
        _ <- assertEvent(QuotedIncomplete(input, 0, "\"", 0))
      } yield ()

      test.run(RowScanner.startLine(input))
    }

    "recognize empty cells" in {
      val input = ","

      val test = for {
        _ <- assertEvent(CellParced(input, 1, ""))
        _ <- assertEvent(CellIncomplete(input, 1, ""))
        _ <- assertEvent(CellParced(input, 2, ""))
        _ <- assertEvent(RowComplete(input, 2))
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
      carryOver shouldBe Some(QuotedIncomplete(input, 17, "\"e,g,h", 17))
    }

    "scan with alternative delimiters" in {
      val input = "'ccc:ddd' : 'eee',fff "
      val altDelimiters = new Delimiters {
        override val quote = '''
        override val comma = ':'
      }

      val (scannedRow, _) = RowScanner.scanRow(Nil, RowScanner.startLine(input))(altDelimiters)
      scannedRow.reverse shouldBe List("'ccc:ddd' ", " 'eee',fff ")
    }
  }
}
