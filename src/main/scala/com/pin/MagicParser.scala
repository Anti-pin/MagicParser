package com.pin

object MagicParser extends App {

  case class CarryOver(parsed: List[String], quoted: QuotedIncomplete)

  def parsed(input: Stream[String], carryOver: Option[CarryOver] = None)
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
              content = s"${event.content}\n",
              quotePosition = -1))
        }

        val (parsedRow, newCarry) = RowScanner.scanRow(initialCells, initialScannerEvent)

        newCarry match {
          case None =>
            parsedRow.reverse #:: parsed(tail, None)
          case Some(qi) =>
            parsed(tail, Some(CarryOver(parsedRow, qi)))
        }
    }
  }
}