package exercises

import scala.io.BufferedSource
import scala.io.Source

object day7 extends App{

  def readUserInput():Int = {
    //This, as the readInput is not very functional. Consider using some libraries (Cats? ZIO?) and cleaning it a bit
    println("PLEASE, PROVIDE AN INPUT VALUE AND PRESS ENTER")
    scala.io.StdIn.readInt()
  }

  def fromStdin() = {
    io.Source.stdin.bufferedReader()
  }

  def direct() = {

  }

  def run(input_file:String):(Int,Int) = {
    println(fromStdin().readLine())
    (0,0)
  }

  val (result1, result2) = run(args.headOption.getOrElse("inputs/day7.csv"))
  println(s"Day7-1: $result1")
  println(s"Day7-2: $result2")
}
