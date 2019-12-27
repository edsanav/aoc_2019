package exercises

import exercises.IntcodeComputer.{Computer, operation, operationsQueues}
import exercises.day2.readInput

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.BufferedSource
import scala.io.Source

object day7 extends App{

  @tailrec
  def computeModules(modules:Queue[(Computer, Queue[Int])], lastOut:Int):Int = {
    if (modules.isEmpty) lastOut // if no other modules to compute, output
    else{
      val ((computer, in), nextQ) = modules.dequeue // get next computer and input queue, plus the remaining modules
      val trueIn = in:+lastOut // add previous output to input queue
      // Compute module. If End of computation is reached, we get no computer. Else (if stuck in input val without input),
      // we get the state of the computer in that moment plus its output so far. So we add that to the modules
      val (maybeComp, out) = operationsQueues(computer)(trueIn)
      val finalQ = if (maybeComp.isEmpty) nextQ else nextQ:+(maybeComp.get, Queue[Int]())
      // Recursive call to keep computing modules
      computeModules(finalQ, out.last)
    }
  }

  def computeSignal(computer:Computer)(phases:Queue[Int]):Int = {
    val modules = Queue.fill(phases.length)(computer).zip(phases.map(x => Queue(x)))
    computeModules(modules, 0)
  }

  def computeLoopSignal(software:Queue[Int] => Queue[Int])(phases:Queue[Int]):Int = {
    @tailrec
    def go(remainingPhases:Queue[Int], lastOut:Int):Int = {
      val phasesToRun = if (remainingPhases.nonEmpty) remainingPhases else phases //reinitialize phases
      val (phase, newPhases) = phasesToRun.dequeue
      val moduleOut = software(Queue(phase, lastOut))
      if (moduleOut.isEmpty) lastOut
      else go(newPhases, moduleOut.last)
    }
    go(phases, 0)
  }

  def highestSignal(signalFromPhases:Queue[Int] => Int, combine:Queue[Int] = Queue(0,1,2,3,4)):Int = {
    val combinations = combine.permutations.toList
    combinations.tail.foldLeft(signalFromPhases(combinations.head))(
      (accum:Int, ls:Queue[Int])=>{
        val signal = signalFromPhases(ls)
        if (signal >= accum) signal else accum
      }
    )
  }

  def run(input_file:String):(Int,Int) = {
    val rawV = readInput(input_file)
    val software:Queue[Int] => Int = computeSignal(Computer(rawV, 0))
    (highestSignal(software, combine=Queue(0,1,2,3,4)), highestSignal(software, combine=Queue(5,6,7,8,9)))
  }

  val (result1, result2) = run(args.headOption.getOrElse("inputs/day7.csv"))
  println(s"Day7-1: $result1")
  println(s"Day7-2: $result2")
}
