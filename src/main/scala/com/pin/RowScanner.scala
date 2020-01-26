package com.pin

object implicits {
  implicit val delimiters: Delimiters = new Delimiters {}
}

trait ParserEvent {
  val input: String
  val position: Int
}

trait WithContent extends ParserEvent {
  val content: String
}

case class CellParced(input: String, position: Int, content: String) extends ParserEvent with WithContent

case class CellIncomplete(input: String, position: Int, content: String) extends ParserEvent with WithContent

case class Escaped(input: String, position: Int, content: String) extends ParserEvent with WithContent

case class Quoted(input: String, position: Int, content: String, quotePosition: Int) extends ParserEvent with WithContent

case class QuotedEscaped(input: String, position: Int, content: String, quotePosition: Int) extends ParserEvent with WithContent

case class QuotedIncomplete(input: String, position: Int, content: String, quotePosition: Int) extends ParserEvent with WithContent

case class RowComplete(input: String, position: Int) extends ParserEvent

trait Delimiters {
  val quote = '"'
  val cellSeparator = ','
  val escape = '\\'
}

object RowScanner {


  def startLine(input: String): ParserEvent = CellIncomplete(input, 0, "")

  def scan(state: ParserEvent)(implicit delimiters: Delimiters): ParserEvent = {
    state match {
      case CellIncomplete(input, pos, content) =>
        val newPos = input.indexWhere(c => c == delimiters.quote || c == delimiters.cellSeparator || c == delimiters.escape, pos)

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
            case delimiters.cellSeparator =>
              val newContent = input.substring(pos, newPos)
              CellParced(input, newPos + 1, content ++ newContent)
            case delimiters.escape =>
              val newContent = input.substring(pos, newPos)
              Escaped(input, newPos + 1, content ++ newContent)
          }
        }

      case Escaped(input, pos, content) =>
        if (pos < input.length) {
          val newContent = input(pos)
          CellIncomplete(input, pos + 1, content + newContent)
        } else {
          // Escaped line end, undefined behavior. Let's simply add escape character to the value of the cell
          CellParced(input, pos + 1, content + delimiters.escape)
        }


      case Quoted(input, pos, content, quotePosition) =>
        val delimiterPosition = input.indexWhere(c => c == delimiters.quote || c == delimiters.escape, pos + 1)

        if (delimiterPosition == -1) {
          QuotedIncomplete(input, pos, content + input.substring(quotePosition), quotePosition)
        } else {
          input(delimiterPosition) match {
            case delimiters.quote =>
              // content includes quote symbol
              CellIncomplete(input, delimiterPosition + 1, content + input.substring(pos, delimiterPosition + 1))
            case delimiters.escape =>
              // content does not include escape symbol
              QuotedEscaped(input, delimiterPosition + 1, content  + input.substring(pos, delimiterPosition), quotePosition)
          }
        }

      case QuotedEscaped(input, pos, content, quotePosition) =>
        if (pos < input.length) {
          val newContent = input(pos)
          Quoted(input, pos + 1, content + newContent, quotePosition)
        } else {
          // Escaped line end, undefined behavior. Let's simply add escape character to the value of the cell
          CellParced(input, pos + 1, content + delimiters.escape)
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