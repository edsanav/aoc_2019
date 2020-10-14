package exercises

import cats.data.State
import exercises.auxiliar.loadResourceFile
import auxiliar._
import cats.effect.IO

object ex2 {

  val INPUT:String = "inputs/day2.csv"
  val SETUP = List(
    (v:Vector[Int]) => v.updated(1, 12),
    (v:Vector[Int]) => v.updated(2, 2)
  )

  final case class Computer(v:Vector[Int], cursor:Int){
    def value:Int = v(cursor)
  }

  sealed abstract class Instruction{
    def result:Computer
  }
  trait Operation extends Instruction {
    val computer:Computer
    val op:(Int,Int)=>Int
    def result:Computer = {
      val target = computer.v(computer.cursor + 3)
      val idx = computer.v(computer.cursor+1)
      val idy = computer.v(computer.cursor+2)
      val result = op(computer.v(idx), computer.v(idy))
      val nextCursor = computer.cursor + 4
      Computer(computer.v.updated(target, result), nextCursor)
    }
  }
  final case class Sum(computer: Computer) extends Operation {
    override val op:(Int, Int)=>Int = (_ + _)
  }
  final case class Multiplication(computer:Computer) extends Operation {
    override val op:(Int, Int)=>Int = (_ * _)
  }
  final case class Halt(computer:Computer) extends Instruction{
    def result:Computer = computer
  }


  type ComputerState[A] = State[Computer, A]

  def run: IO[Int] = {
    for {
      lines <- loadResourceFile(INPUT).use(getLines)
      initial <- IO(setup(lines.head, SETUP))
      result <- IO(go.runA(initial).value)
    } yield result
  }

  def go:ComputerState[Int] = {
    for {
      inst <- State.inspect[Computer, Instruction](instruction)
      outInt <- inst match {
        case h:Halt => State.inspect[Computer, Int](_ => h.result.v(0))
        case op:Operation => for {
          _ <- State.set(op.result)
          res <- go
        } yield res
      }
    } yield outInt
  }


  def setup(line:String, operations:List[Vector[Int] => Vector[Int]]):Computer = {
    val initial = line.split(",").map(_.toInt).toVector
    Computer(operations.foldLeft(initial){case (v, op) => op(v)}, 0)
  }

  def instruction(computer:Computer):Instruction = computer.value match {
        case 99 => Halt(computer)
        case 1 => Sum(computer)
        case 2 => Multiplication(computer)
        case _ => throw new UnsupportedOperationException(s"${computer.value} is not a valid operation code")
      }

}
