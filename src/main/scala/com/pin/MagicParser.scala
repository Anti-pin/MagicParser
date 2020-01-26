package com.pin

import scala.collection.immutable.Stream.Empty
import scala.io.{BufferedSource, Source}

object MagicParser {

  case class CarryOver(parsed: List[String], quoted: QuotedIncomplete)

  /**
   * Although this function is recursive (and not tail recursive) it is actually (almost) stack safe as it deals with
   * lazy streams rather than eager sequences.
   * 'Almost' because an enormous unclosed quote may, in theory, blow it
   */
  def parse(input: Stream[String], carryOver: Option[CarryOver] = None)
           (implicit delimiters: Delimiters): Stream[Seq[String]] = {

    input match {
      case Stream.Empty =>
        Stream.Empty

      case head #:: tail =>
        val (initialCells, initialScannerEvent) = carryOver match {
          case None =>
            (Nil, CellIncomplete(head, 0, ""))
          case Some(CarryOver(cells, event)) =>
            (cells, Quoted(input = head,
              position = 0,
              content = s"${event.content}${delimiters.lineSeparator}"))
        }

        val (parsedRow, newCarry) = RowScanner.scanRow(initialCells, initialScannerEvent)

        newCarry match {
          case None =>
            parsedRow.reverse #:: parse(tail, None)
          case Some(qi) =>
            parse(tail, Some(CarryOver(parsedRow, qi)))
        }
    }
  }

  def parseWithHeader(input: Stream[String])(implicit delimiters: Delimiters): Stream[Seq[(String, String)]] = {

    val parsedStream = parse(input)

    parsedStream match {
      case Empty => Empty
      case _ #:: Empty => Empty
      case header #:: tail =>
        tail.map {
          row: Seq[String] =>
            val lengthD = header.length - row.length
            val lengthAdjusted = if (lengthD > 0)
              row ++ Seq.fill(lengthD)("")
            else
              row
            header.zip(lengthAdjusted)
        }
    }
  }
}


object MagicParserApp extends App {

  import implicits.defaultDelimiters

  val source: BufferedSource = Source.fromFile(args(0), "UTF-8")
  val inputStream = source.getLines().toStream


  MagicParser.parseWithHeader(inputStream).foreach(println)
  source.close()

}