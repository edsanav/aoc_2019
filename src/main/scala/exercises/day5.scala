package exercises

import IntcodeComputer._

import scala.collection.immutable.Queue


object day5 extends App {

  def run(input_file:String):(BigInt,BigInt) = {
    val rawV = readBigIntsVector(input_file)
    def operationsWithInput:Queue[BigInt] => (Option[Computer], Queue[BigInt]) = operationsQueues(Computer(rawV,0))
    val (_, firstOut) = operationsWithInput(Queue(BigInt(1)))
    val (_, secondOut) = operationsWithInput(Queue(BigInt(5)))
    (firstOut.last,secondOut.last)
  }


  val (result1, result2) = run(args.headOption.getOrElse("inputs/day5.csv"))
  println(s"Day5-1: $result1")
  println(s"Day5-2: $result2")


}
