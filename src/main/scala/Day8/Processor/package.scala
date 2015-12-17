package Day8

package object Processor {
  import scala.language.implicitConversions

  val ControlCharacter = '^'

  sealed trait TextProcessor {
    def process(s: String): String
  }

  object Decoder extends TextProcessor {
    override def process(s: String): String = parseText(s.filter(_ != ControlCharacter), "", "")

    private val hexRegex = "^[a-f0-9]$".r
    private val partialAsciiRegex = """^\\x[a-f0-9]?""".r

    private def isPartialAscii(s: String): Boolean = partialAsciiRegex.findFirstMatchIn(s).isDefined

    private def getAsciiChar(s: String): Char = {
      implicit def hex2int (hex: String): Int = Integer.parseInt(hex, 16)
      val intVal: Int = s.slice(2, 4)
      intVal.toChar
    }

    private def parseText(source: String, out: String, buffer: String): String =
      if (source.isEmpty) out
      else source.head match {
        case '"' if buffer.isEmpty => parseText(source.tail, out, "")
        case '\\' if buffer.isEmpty => parseText(source.tail, out, source.head.toString)
        case 'x' if buffer == "\\" => parseText(source.tail, out, buffer + source.head)
        case hexRegex() if isPartialAscii(buffer) => {
          val newBuffer = buffer + source.head
          if (newBuffer.length == 4) parseText(source.tail, out + getAsciiChar(newBuffer), "")
          else parseText(source.tail, out, newBuffer)
        }
        case _ => parseText(source.tail, out + source.head, "")
      }
  }

  object Encoder extends TextProcessor {
    override def process(s: String): String = encodeText(s, "\"")

    private def encodeText(source: String, out: String): String =
      if (source.isEmpty) out + "\""
      else source.head match {
        case '"' => encodeText(source.tail, out + """ \" """.trim)
        case '\\' => encodeText(source.tail, out + """ \\ """.trim)
        case ControlCharacter => encodeText(source.tail, out + "\"\"")
        case _ => encodeText(source.tail, out + source.head)
      }
  }
}
