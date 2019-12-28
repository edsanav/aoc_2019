package exercises

import scala.annotation.tailrec
import scala.io.Source
import IntcodeComputer._


object day2 extends App{


  def compute(noun:Int, verb:Int, rawV:Vector[BigInt]):BigInt = {
    val startV = rawV updated(1, BigInt(noun)) updated (2, BigInt(verb))
    val finalComputer = operationFromUserInput(Computer(startV, 0))
    finalComputer.v(0)
  }

  def findCombination(v:Vector[BigInt]):LazyList[(Int,Int,BigInt)] = {
    for {
      x <- LazyList range(0, 99)
      y <- LazyList range(0, 99)
    } yield (x,y, compute(x, y, v))
  }

  def run(input_file:String):(BigInt,BigInt) = {
    val rawV = readBigIntsVector(input_file)
    val part1 = compute(12, 2, rawV)
    val (noun, verb, _) = findCombination(rawV).find( x => x._3 == 19690720).get
    val part2 = (noun*100 + verb)
    (part1, part2)
  }

  val (result1, result2) = run(args.headOption.getOrElse("inputs/day2.csv"))
  println(s"Day2-1: $result1")
  println(s"Day2-2: $result2")

}
