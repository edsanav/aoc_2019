package exercises

import cats.data.State
import exercises.auxiliar.loadResourceFile
import auxiliar._
import cats.Semigroupal
import cats.effect.IO
import cats.syntax.show._

object ex2 {

  val INPUT:String = "inputs/day2.csv"

  val EXPECTED = 19690720

  final case class Computer(mem:Vector[Int], cursor:Int=0){
    def updated(pos:Int, newVal:Int):Computer = Computer(mem.updated(pos, newVal), cursor)
    def initialize(noun:Int, verb:Int):Computer = updated(1, noun).updated(2, verb)
    def value:Int = mem(cursor)
  }

  sealed abstract class Instruction{
    val opCode:Int
    def result:Computer
  }
  trait Operation extends Instruction {
    val computer:Computer
    val op:(Int,Int)=>Int
    def result:Computer = {
      val target = computer.mem(computer.cursor + 3)
      val idx = computer.mem(computer.cursor+1)
      val idy = computer.mem(computer.cursor+2)
      val result = op(computer.mem(idx), computer.mem(idy))
      val nextCursor = computer.cursor + 4
      Computer(computer.mem.updated(target, result), nextCursor)
    }
  }
  final case class Sum(computer: Computer) extends Operation {
    override val opCode: Int = 1
    override val op:(Int, Int)=>Int = (_ + _)
  }
  final case class Multiplication(computer:Computer) extends Operation {
    override val opCode: Int = 2
    override val op:(Int, Int)=>Int = (_ * _)
  }
  final case class Halt(computer:Computer) extends Instruction{
    override val opCode: Int = 99
    def result:Computer = computer
  }


  type ComputerState[A] = State[Computer, A]

  def run: IO[String] = {
    // TODO parellize
    // TODO shouldn't be throwing execeptions around, should use Either[String, A] or something
    // TODO functions probably should return State[Either[ so final stack should be something like
    //  IO[State[Either[String, ComputerState[A]]] and work with EitherT[StateT[IO...]
    // TODO check https://typelevel.org/cats-mtl/getting-started.html
    for {
      lines <- loadResourceFile(INPUT).use(getLines)
      computer <- IO(Computer(lines.head.split(",").map(_.toInt).toVector))
      result <- IO(execute(computer.initialize(12, 2)))
      combination <-IO(findInitValues(computer, EXPECTED))
    } yield (result, combination).show
  }



  def go:ComputerState[Int] = {
    for {
      inst <- State.inspect[Computer, Instruction](instruction)
      outInt <- inst match {
        case h:Halt => State.inspect[Computer, Int](_ => h.result.mem(0))
        case op:Operation => for {
          _ <- State.set(op.result)
          res <- go
        } yield res
      }
    } yield outInt
  }

  def execute(computer:Computer):Int = go.runA(computer).value


  def findInitValues(computer:Computer, expected:Int):Int = {
    val combinations = Semigroupal[LazyList].product(LazyList.range(1,100), LazyList.range(1,100))
    val initValues = combinations.find{case (noun, verb) => execute(computer.initialize(noun, verb)) == expected }
    initValues match {
      case Some((noun, verb)) =>  100 * noun + verb
      case None => throw new RuntimeException("Combination not found")
    }
  }



  def instruction(computer:Computer):Instruction = computer.value match {
        case 99 => Halt(computer)
        case 1 => Sum(computer)
        case 2 => Multiplication(computer)
        case _ => throw new UnsupportedOperationException(s"${computer.value} is not a valid operation code")
      }

}
