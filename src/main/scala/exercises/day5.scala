package exercises

import exercises.day2._


object day5 extends App {

  def run(input_file:String):(Int,Int) = {
    val rawV = readInput(input_file)
//    val firstRun = operation(rawV, 0)
    val secondRun = operation(rawV, 0)
    (0,0)
  }


  val (result1, result2) = run(args.headOption.getOrElse("inputs/day5.csv"))
  println(s"Day5-1: $result1")
  println(s"Day5-2: $result2")


}
