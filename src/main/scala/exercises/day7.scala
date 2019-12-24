package exercises

import exercises.IntcodeComputer.operation

import scala.collection.immutable.Queue
import scala.io.BufferedSource
import scala.io.Source

object day7 extends App{


  def run(input_file:String):(Int,Int) = {
//    val secondRun = operation(5)(Queue[Int]())(rawV, 0)
    (0,0)
  }

  val (result1, result2) = run(args.headOption.getOrElse("inputs/day7.csv"))
  println(s"Day7-1: $result1")
  println(s"Day7-2: $result2")
}
