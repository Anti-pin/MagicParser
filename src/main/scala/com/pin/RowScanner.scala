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

trait IntermediateParserEvent extends ParserEvent with WithContent {
  def nextEvent(implicit delimiters: Delimiters): ParserEvent
}

case class CellParced(input: String, position: Int, content: String) extends IntermediateParserEvent {
  override def nextEvent(implicit delimiters: Delimiters): ParserEvent = {
    if (position > input.length)
      RowComplete(input, position)
    else
      CellIncomplete(input, position, "")
  }
}

case class CellIncomplete(input: String, position: Int, content: String) extends IntermediateParserEvent {

  override def nextEvent(implicit delimiters: Delimiters): ParserEvent = {
    val newPos = input.indexWhere(c => c == delimiters.quote || c == delimiters.cellSeparator || c == delimiters.escape, position)
    if (newPos == -1) {
      // end of line
      val cellValue = input.substring(position)
      CellParced(input, input.length + 1, content ++ cellValue)
    } else {
      val delimiter = input(newPos)

      delimiter match {
        case delimiters.quote =>
          if (newPos != position) {
            val newContent = input.substring(position, newPos)
            CellIncomplete(input, newPos, content ++ newContent)
          } else
            Quoted(input, position, content)
        case delimiters.cellSeparator =>
          val newContent = input.substring(position, newPos)
          CellParced(input, newPos + 1, content ++ newContent)
        case delimiters.escape =>
          val newContent = input.substring(position, newPos)
          Escaped(input, newPos + 1, content ++ newContent)
      }
    }
  }
}

case class Escaped(input: String, position: Int, content: String) extends IntermediateParserEvent {
  override def nextEvent(implicit delimiters: Delimiters): ParserEvent = {
    if (position < input.length) {
      val newContent = input(position)
      CellIncomplete(input, position + 1, content + newContent)
    } else {
      // Escaped line end, undefined behavior. Let's simply add escape character to the value of the cell
      CellParced(input, position + 1, content + delimiters.escape)
    }
  }
}

case class Quoted(input: String, position: Int, content: String) extends IntermediateParserEvent {
  override def nextEvent(implicit delimiters: Delimiters): ParserEvent = {
    val delimiterPosition = input.indexWhere(c => c == delimiters.quote || c == delimiters.escape, position + 1)

    if (delimiterPosition == -1) {
      QuotedIncomplete(input, position, content + input.substring(position))
    } else {
      input(delimiterPosition) match {
        case delimiters.quote =>
          // content includes quote symbol
          CellIncomplete(input, delimiterPosition + 1, content + input.substring(position, delimiterPosition + 1))
        case delimiters.escape =>
          // content does not include escape symbol
          QuotedEscaped(input, delimiterPosition + 1, content + input.substring(position, delimiterPosition))
      }
    }
  }
}

case class QuotedEscaped(input: String, position: Int, content: String) extends IntermediateParserEvent {
  override def nextEvent(implicit delimiters: Delimiters): ParserEvent = {
    if (position < input.length) {
      val newContent = input(position)
      Quoted(input, position + 1, content + newContent)
    } else {
      // Escaped line end, undefined behavior. Let's simply add escape character to the value of the cell
      CellParced(input, position + 1, content + delimiters.escape)
    }
  }
}

case class QuotedIncomplete(input: String, position: Int, content: String) extends ParserEvent with WithContent

case class RowComplete(input: String, position: Int) extends ParserEvent

trait Delimiters {
  val quote = '"'
  val cellSeparator = ','
  val escape = '\\'
}

object RowScanner {


  def startLine(input: String): ParserEvent = CellIncomplete(input, 0, "")

  // Partial, not defined for not intermediate events
  def scan(state: ParserEvent)(implicit delimiters: Delimiters): ParserEvent = state match {
    case ipe: IntermediateParserEvent => ipe.nextEvent
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