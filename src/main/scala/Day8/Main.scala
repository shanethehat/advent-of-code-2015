package Day8

import Day8.Processor.{Encoder, Decoder, ControlCharacter}

import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("data/day8.txt").getLines()

  val controlText = lines.foldLeft("")((t, l) => t + l + ControlCharacter).dropRight(1)
  val fullText = controlText.filter(_ != ControlCharacter)

  val realText = Decoder.process(controlText)
  val encoded = Encoder.process(controlText)

  println(fullText.length - realText.length)
  println(encoded.length - fullText.length)
}
