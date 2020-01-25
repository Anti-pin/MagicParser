package com.pin

trait ParserEvent {
  val input: String
  val position: Int
}

case class CellParced(input: String, position: Int, content: String) extends ParserEvent

case class CellIncomplete(input: String, position: Int, content: String) extends ParserEvent

case class Quoted(input: String, position: Int, content: String, quotePosition: Int) extends ParserEvent

case class QuotedIncomplete(input: String, position: Int, content: String, quotePosition: Int) extends ParserEvent

case class RowComplete(input: String, position: Int) extends ParserEvent

object RowScanner {
  private val quote = '"'
  private val comma = ','


  def startLine(input: String): ParserEvent = CellIncomplete(input, 0, "")

  def scan(state: ParserEvent): ParserEvent = {
    state match {
      case CellIncomplete(input, pos, content) =>

        val newPos = input.indexWhere(c => c == quote || c == comma, pos)

        if (newPos == -1) {
          // end of line
          val cellValue = input.substring(pos)
          CellParced(input, input.length + 1, content ++ cellValue)
        } else {
          val delimiter = input(newPos)

          delimiter match {
            case RowScanner.quote =>
              if (newPos != pos) {
                val newContent = input.substring(pos, newPos)
                CellIncomplete(input, newPos, content ++ newContent)
              } else
                Quoted(input, pos, content, newPos)
            case RowScanner.comma =>
              val newContent = input.substring(pos, newPos)
              CellParced(input, newPos + 1, content ++ newContent)
          }
        }

      case Quoted(input, pos, content, quotePosition) =>
        val closingQuotePos = input.indexOf(quote, quotePosition + 1)
        if (closingQuotePos == -1) {
          QuotedIncomplete(input, pos, content + input.substring(quotePosition), quotePosition)
        } else {
          CellIncomplete(input, closingQuotePos + 1, content + input.substring(pos, closingQuotePos + 1))
        }

      case CellParced(input, position, _) =>
        if (position > input.length)
          RowComplete(input, position)
        else
          CellIncomplete(input, position, "")
    }
  }


  @scala.annotation.tailrec
  def scanRow(parsed: List[String], event: ParserEvent): (List[String], Option[QuotedIncomplete]) = {
    scan(event) match {
      case e@CellParced(_, _, content) =>
        scanRow(content :: parsed, e)
      case _: RowComplete =>
        (parsed, None)
      case qi: QuotedIncomplete =>
        (parsed, Some(qi))
      case e: ParserEvent =>
        scanRow(parsed, e)
    }
  }
}