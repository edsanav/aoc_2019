package exercises

import cats.data.State

object ex2 {

  val INPUT:String = "inputs/day2.csv"

  final case class Computer(v:Vector[Int], cursor:Int)

  type ComputerState[A] = State[Computer, A]


  sealed abstract class Operation
  final case class Sum(a:Int, b:Int) extends Operation
  final case class Multiplication(a:Int, b:Int) extends Operation
  final case class Halt() extends Operation


  def frame(opCode:Int, idx:Int, idy:Int, target:Int):ComputerState[Int] = {
    State[Computer, Int]{
      (comp:Computer) => {
        val op = toOperation(opCode)
        val newCursor = comp.cursor + 4
        (Computer(comp.v.updated(target, op(comp.v(idx), comp.v(idy))), comp.cursor+4), comp.v(newCursor))
        }
    }
  }

  def toOperation(opCode:Int):(Int, Int) => Int ={
    opCode match {
      case 1 => (_ + _)
      case 2 => (_ * _)
      case _ => throw new UnsupportedOperationException(s"$opCode is not a valid operation code")
    }
  }

}
