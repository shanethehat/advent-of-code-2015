package Day7

import scala.io.Source


object Main extends App {

  sealed trait Provider {
    def signal(circuit:Map[String, Provider]): Int
  }

  case class Wire(p: String) extends Provider {
    def signal(circuit:Map[String, Provider]): Int = circuit(p).signal(circuit)
  }

  case class Value(value: Int) extends Provider {
    def signal(circuit:Map[String, Provider]): Int = value
  }

  case class Gate(providers: Seq[String], operation: Seq[Int] => Int) extends Provider {
    def signal(circuit:Map[String, Provider]): Int = operation(providers.map(circuit(_).signal(circuit)))
  }

  val notRegex = """NOT ([a-z]{1,3}) -> ([a-z]{1,3})""".r
  val orRegex = """([a-z]{1,3}) OR ([a-z]{1,3}) -> ([a-z]{1,3})""".r
  val andRegex = """([a-z]{1,3}|1) AND ([a-z]{1,3}) -> ([a-z]{1,3})""".r
  val lshiftRegex = """([a-z]{1,3}) LSHIFT (\d*) -> ([a-z]{1,3})""".r
  val rshiftRegex = """([a-z]{1,3}) RSHIFT (\d*) -> ([a-z]{1,3})""".r
  val valueRegex = """(\d*) -> ([a-z]{1,3})""".r
  val patchRegex = """([a-z]{1,3}) -> ([a-z]{1,3})""".r

  def updateCircuitFromLine(line: String, circuit: Map[String, Provider]): Map[String, Provider] = line match {
    case notRegex(in, out) => updateCircuitFromNot(in, out, circuit)
    case orRegex(in1, in2, out) => updateCircuitFromOr(in1, in2, out, circuit)
    case andRegex(in1, in2, out) => updateCircuitFromAnd(in1, in2, out, circuit)
    case lshiftRegex(in1, in2, out) => updateCircuitFromLshift(in1, in2, out, circuit)
    case rshiftRegex(in1, in2, out) => updateCircuitFromRshift(in1, in2, out, circuit)
    case valueRegex(in, out) => updateCircuitFromValue(in, out, circuit)
    case patchRegex(in, out) => updateCircuitFromPatch(in, out, circuit)
  }

  def updateCircuitFromNot(in: String, out:String, circuit: Map[String, Provider]): Map[String, Provider] = {
    val gate = Gate(Seq(in), is => 65536 + ~is.head)
    val gateId = in + "NOT" + out
    val circuitWithGate = circuit ++ Map(gateId -> gate)
    circuitWithGate ++ Map(out -> Wire(gateId))
  }

  def updateCircuitFromOr(in1: String, in2: String, out: String, circuit: Map[String, Provider]): Map[String, Provider] = {
    val gate = Gate(Seq(in1, in2), is => is.head | is.tail.head)
    val gateId = in1 + in2 + "OR" + out
    val circuitWithGate = circuit ++ Map(gateId -> gate)
    circuitWithGate ++ Map(out -> Wire(gateId))
  }

  def updateCircuitFromAnd(in1: String, in2: String, out: String, circuit: Map[String, Provider]): Map[String, Provider] = {
    val inLeft: String = if (in1 == "1") in1+in2 else in1
    val circuitWithValue:Map[String, Provider] = if (in1 == "1") circuit ++ Map(inLeft ->Value(1)) else circuit
    val gate = Gate(Seq(inLeft, in2), is => is.head & is.tail.head)
    val gateId = in1 + in2 + "AND" + out
    val circuitWithGate = circuitWithValue ++ Map(gateId -> gate)
    circuitWithGate ++ Map(out -> Wire(gateId))
  }

  def updateCircuitFromLshift(in1: String, in2: String, out: String, circuit: Map[String, Provider]): Map[String, Provider] = {
    val gate = Gate(Seq(in1), is => is.head << in2.toInt)
    val gateId = in1 + in2 + "LSHIFT" + out
    val circuitWithGate = circuit ++ Map(gateId -> gate)
    circuitWithGate ++ Map(out -> Wire(gateId))
  }

  def updateCircuitFromRshift(in1: String, in2: String, out: String, circuit: Map[String, Provider]): Map[String, Provider] = {
    val gate = Gate(Seq(in1), is => is.head >> in2.toInt)
    val gateId = in1 + in2 + "RSHIFT" + out
    val circuitWithGate = circuit ++ Map(gateId -> gate)
    circuitWithGate ++ Map(out -> Wire(gateId))
  }

  def updateCircuitFromValue(in: String, out:String, circuit: Map[String, Provider]): Map[String, Provider] = {
    circuit ++ Map(out -> Value(in.toInt))
  }

  def updateCircuitFromPatch(in: String, out:String, circuit: Map[String, Provider]): Map[String, Provider] = {
    circuit ++ Map(out -> Wire(in))
  }

  val lines = Source.fromFile("data/day7.txt").getLines().toList
  val circuit = lines.foldLeft(Map[String, Provider]())((c, l) => updateCircuitFromLine(l, c))
  println(circuit("a").signal(circuit))
//  println(circuit)
//  println(circuit("d").signal(circuit))
//  println(circuit("e").signal(circuit))
//  println(circuit("f").signal(circuit))
//  println(circuit("g").signal(circuit))
//  println(circuit("h").signal(circuit))
//  println(circuit("i").signal(circuit))
//  println(circuit("x").signal(circuit))
//  println(circuit("y").signal(circuit))

}
