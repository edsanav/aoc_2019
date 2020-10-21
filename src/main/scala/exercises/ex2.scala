package exercises

import cats.data.{EitherT, State}
import cats.instances.either._ // for Monad
import exercises.auxiliar.loadResourceFile
import auxiliar._
import cats.Semigroupal
import cats.effect.IO
import cats.syntax.show._

object ex2 {

  val INPUT:String = "inputs/day2.csv"
  val EXPECTED = 19690720

  type ComputerState[A] = State[Computer, A]
  type ComputerStateEither[A] = EitherT[ComputerState, String, A]
  type Result[A] = Either[String , A] //TODO use proper errors not just strings



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
//      computer <- IO(Computer(Vector(2,4,4,5,99,0)))
//      result <- IO(execute(computer))
      combination <-IO(findInitValues(computer, EXPECTED))
    } yield (result, combination).show
  }

  // A note:
  // Probably using State is an overkill, and using monad transformers even more, but, just for testing purposes
  // TODO clean this mess
  def go:ComputerStateEither[Int] = {
    for {
      inst <- EitherT[ComputerState, String, Instruction](State.inspect[Computer, Either[String, Instruction]](instruction))
      outInt <- {
        val out:State[Computer, Either[String, Int]] = inst match {
          case h:Halt =>  State.inspect[Computer,  Either[String, Int]](_ => Right(h.result.mem(0)))
          case op:Operation => for {
            _ <- State.set(op.result)
            res <- go.value
          } yield res
        }
        EitherT[ComputerState, String, Int](out)
      }
    } yield outInt
  }

  def execute(computer:Computer):Result[Int] = go.value.runA(computer).value


  def findInitValues(computer:Computer, expected:Int):Result[Int] = {
    val combinations = Semigroupal[LazyList].product(LazyList.range(1,100), LazyList.range(1,100))
    val initValues = for {
      (noun,verb) <- combinations
      result <- go.value.runA(computer.initialize(noun, verb)).value.toOption //This will work but will swallow the error
      if (result==expected)
    } yield (noun, verb)

    initValues.headOption match {
      case Some((noun, verb)) =>  Right(100 * noun + verb)
      case None => Left("Combination not found")
    }
  }


  def instruction(computer:Computer):Result[Instruction] = computer.value match {
        case 99 => Right(Halt(computer))
        case 1 => Right(Sum(computer))
        case 2 => Right(Multiplication(computer))
        case _ => Left(s"${computer.value} is not a valid operation code")
      }

}
