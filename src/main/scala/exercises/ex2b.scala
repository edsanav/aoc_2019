package exercises

import cats.data.State
import exercises.auxiliar.loadResourceFile
import auxiliar._
import cats.effect.IO

object ex2b {

  val INPUT:String = "inputs/day2.csv"
  val SETUP = List(
    (v:Vector[Int]) => v.updated(1, 12),
    (v:Vector[Int]) => v.updated(2, 2)
  )

  final case class Computer(v:Vector[Int], cursor:Int){
    def value:Int = v(cursor)
  }

  type ComputerState[A] = State[Computer, A]

  def run: IO[Int] = {
    for {
      lines <- loadResourceFile(INPUT).use(getLines)
      initial <- IO(setup(lines.head, SETUP))
      result <- IO(go.runA(initial).value)
    } yield result
  }


  def computeNext(opCode:Int):ComputerState[Int] = {
    State[Computer, Int]((c:Computer) => {
      val target = c.cursor + 3
      val result = operation(opCode)(c.v(c.cursor + 1), c.v(c.cursor +2))
      val nextCursor = c.cursor + 4
      val newC = Computer(c.v.updated(target, result), nextCursor)
      (newC, newC.value)
     }
    )
  }

  //https://stackoverflow.com/questions/52273793/recursions-with-the-state-monad
  // FIXME getting out the outer context val, we need the inner most one.
  def go:ComputerState[Int] = {
    for {
      c <- State.get[Computer]
      c2 <- {c.value match {
        case 99 => State.inspect[Computer, Int](c => {println("OUT",c.value); c.v(0)})
        case x => println("current val",x); for {
          _ <- computeNext(x)
          c3 <- go
        } yield c3
      }}
    } yield c2
  }


  def setup(line:String, operations:List[Vector[Int] => Vector[Int]]):Computer = {
    val initial = line.split(",").map(_.toInt).toVector
    Computer(operations.foldLeft(initial){case (v, op) => op(v)}, 0)
  }

  def operation(opCode:Int):(Int, Int) => Int ={
    opCode match {
      case 1 => (_ + _)
      case 2 => (_ * _)
      case _ => throw new UnsupportedOperationException(s"$opCode is not a valid operation code")
    }
  }

}
