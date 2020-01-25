package com.pin

object implicits {
  implicit val delimiters: Delimiters = new Delimiters {}
}

trait ParserEvent {
  val input: String
  val position: Int
}

case class CellParced(input: String, position: Int, content: String) extends ParserEvent

case class CellIncomplete(input: String, position: Int, content: String) extends ParserEvent

case class Quoted(input: String, position: Int, content: String, quotePosition: Int) extends ParserEvent

case class QuotedIncomplete(input: String, position: Int, content: String, quotePosition: Int) extends ParserEvent

case class RowComplete(input: String, position: Int) extends ParserEvent

trait Delimiters {
  val quote = '"'
  val comma = ','
}

object RowScanner {


  def startLine(input: String): ParserEvent = CellIncomplete(input, 0, "")

  def scan(state: ParserEvent)(implicit delimiters: Delimiters): ParserEvent = {
    state match {
      case CellIncomplete(input, pos, content) =>

        val newPos = input.indexWhere(c => c == delimiters.quote || c == delimiters.comma, pos)

        if (newPos == -1) {
          // end of line
          val cellValue = input.substring(pos)
          CellParced(input, input.length + 1, content ++ cellValue)
        } else {
          val delimiter = input(newPos)

          delimiter match {
            case delimiters.quote =>
              if (newPos != pos) {
                val newContent = input.substring(pos, newPos)
                CellIncomplete(input, newPos, content ++ newContent)
              } else
                Quoted(input, pos, content, newPos)
            case delimiters.comma =>
              val newContent = input.substring(pos, newPos)
              CellParced(input, newPos + 1, content ++ newContent)
          }
        }

      case Quoted(input, pos, content, quotePosition) =>
        val closingQuotePos = input.indexOf(delimiters.quote, quotePosition + 1)
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
  def scanRow(parsed: List[String], event: ParserEvent)
             (implicit delimiters: Delimiters): (List[String], Option[QuotedIncomplete]) = {
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