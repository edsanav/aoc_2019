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
    def idx:Int = v(cursor+1)
    def idy:Int = v(cursor+2)
    def target:Int = v(cursor+3)

    def next:Operation = {
      value match {
        case 99 => Halt
        case 1 => Sum(v(idx), v(idy))
        case 2 => Multiplication(v(idx), v(idy))
      }
    }

    def execute:Computer = {
      next match {
        case sum:Sum => Computer(v.updated(target, sum.result), cursor+4)
        case prod:Multiplication => Computer(v.updated(target, prod.result), cursor+4)
        case _ => throw new IllegalArgumentException(s"Invalid operation at position $cursor")
      }
    }

  }

  type ComputerState[A] = State[Computer, A]

  def run: IO[Int] = {
    for {
      lines <- loadResourceFile(INPUT).use(getLines)
      initial <- IO(setup(lines.head, SETUP))
      result <- IO(go.runA(Computer(initial, 0)).value)
    } yield result
  }

  sealed abstract class Operation
  final case class Sum(a:Int, b:Int) extends Operation{
    val result:Int = a+b
  }
  final case class Multiplication(a:Int, b:Int) extends Operation{
    val result:Int = a*b
  }
  case object Halt extends Operation


  //TODO this seems kind of wrong
  def go:ComputerState[Int] = {
    State[Computer, Int]{
      (comp:Computer) =>
        comp.next match {
          case Halt => (comp, comp.v(0))
          case _:Sum|_:Product =>
            val newComp = comp.execute
            go.run(newComp).value
        }
    }
  }

  def setup(line:String, operations:List[Vector[Int] => Vector[Int]]):Vector[Int] = {
    val initial = line.split(",").map(_.toInt).toVector
    operations.foldLeft(initial){case (v, op) => op(v)}
  }

  def toOperation(opCode:Int):(Int, Int) => Int ={
    opCode match {
      case 1 => (_ + _)
      case 2 => (_ * _)
      case _ => throw new UnsupportedOperationException(s"$opCode is not a valid operation code")
    }
  }

}
