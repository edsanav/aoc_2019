package exercises

import exercises.day2._
import IntcodeComputer._

import scala.collection.immutable.Queue


object day5 extends App {

  def run(input_file:String):(Int,Int) = {
    val rawV = readInput(input_file)
    def operationsWithInput:Queue[Int] => (Option[Computer], Queue[Int]) = operationsQueues(Computer(rawV,0))
    val (_, firstOut) = operationsWithInput(Queue(1))
    val (_, secondOut) = operationsWithInput(Queue(5))
    (firstOut.last,secondOut.last)
  }


  val (result1, result2) = run(args.headOption.getOrElse("inputs/day5.csv"))
  println(s"Day5-1: $result1")
  println(s"Day5-2: $result2")


}
