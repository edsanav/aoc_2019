package exercises

import exercises.IntcodeComputer.{Computer, operation, operationsQueues}
import exercises.day2.readInput

import scala.collection.immutable.Queue
import scala.io.BufferedSource
import scala.io.Source

object day7 extends App{


  def computeSignal(initC:Computer)(phases:List[Int]):Int = {
    def operationsWithInput:Queue[Int] => (Computer, Queue[Int]) = operationsQueues(initC)
    val (_, out1) = operationsWithInput(Queue(phases(0), 0))
    val (_, out2)= operationsWithInput(Queue(phases(1), out1.last))
    val (_, out3) = operationsWithInput(Queue(phases(2), out2.last))
    val (_, out4) = operationsWithInput(Queue(phases(3), out3.last))
    val (_, out5) = operationsWithInput(Queue(phases(4), out4.last))
    out5.last
  }

  def computeSignalFL(initC:Computer)(phases:List[Int]):Int = {
    def operationsWithInput:Queue[Int] => (Computer, Queue[Int]) = operationsQueues(initC)
    val (_, out) = phases.tail.foldLeft(operationsWithInput(Queue(phases.head,0))){
      (lastOut:(Computer,Queue[Int]),phase:Int) => operationsWithInput(Queue(phase, lastOut._2.last))
    }
    out.last
  }

  def highestSignal(initC:Computer):Int = {
    val combinations = List(0,1,2,3,4).permutations.toList
    val computeSignalWithPhase:List[Int] => Int = computeSignalFL(initC)
    combinations.tail.foldLeft(computeSignalWithPhase(combinations.head))(
      (accum:Int, ls:List[Int])=>{
        val signal = computeSignalWithPhase(ls)
        if (signal >= accum) signal else accum
      }
    )
  }

  def run(input_file:String):(Int,Int) = {
    val rawV = readInput(input_file)
    val highest = highestSignal(Computer(rawV, 0))
    (highest,0)
  }

  val (result1, result2) = run(args.headOption.getOrElse("inputs/day7.csv"))
  println(s"Day7-1: $result1")
  println(s"Day7-2: $result2")
}
