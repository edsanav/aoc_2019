package exercises

import exercises.day2._
import IntcodeComputer._

import scala.collection.immutable.Queue


object day5 extends App {

  def inputQueue: State[Int, Queue[Int]] = State((q:Queue[Int]) => {
    val out = q.dequeue
    (out._2, out._1)
  })

  def run(input_file:String):(Int,Int) = {
    val rawV = readInput(input_file)
    val firstRun = operation(1)(Queue[Int]())(rawV, 0)
    val secondRun = operation(5)(Queue[Int]())(rawV, 0)
    (firstRun._3.last,secondRun._3.last)
  }


  val (result1, result2) = run(args.headOption.getOrElse("inputs/day5.csv"))
  println(s"Day5-1: $result1")
  println(s"Day5-2: $result2")


}
