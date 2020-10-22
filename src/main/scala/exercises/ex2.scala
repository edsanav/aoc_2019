package exercises

import cats.data.{EitherT, State}
import cats.instances.either._ // for Monad
import exercises.auxiliar.loadResourceFile
import auxiliar._
import cats.Semigroupal
import cats.effect.IO
import cats.syntax.show._

object ex2 {

  val INPUT: String = "inputs/day2.csv"
  val EXPECTED = 19690720

  type ComputerState[A] = State[Computer, A]
  type ComputerStateEither[A] = EitherT[ComputerState, String, A]
  type Result[A] = Either[String, A] //TODO use proper errors not just strings


  final case class Computer(mem: Vector[Int], cursor: Int = 0) {
    def updated(pos: Int, newVal: Int): Computer = Computer(mem.updated(pos, newVal), cursor)

    def initialize(noun: Int, verb: Int): Computer = updated(1, noun).updated(2, verb)

    def value: Int = mem(cursor)

    def apply(x:Int):Int = mem(x)
  }

  sealed abstract class Instruction {
    val opCode: Int

    def result: Computer
  }

  trait Operation extends Instruction {
    val computer: Computer
    val op: (Int, Int) => Int

    // TODO Computer change it to Result[Computer]
    def result: Computer = {
      val target = computer(computer.cursor + 3)
      val idx = computer(computer.cursor + 1)
      val idy = computer(computer.cursor + 2)
      val result = op(computer(idx), computer(idy))
      val nextCursor = computer.cursor + 4
      Computer(computer.mem.updated(target, result), nextCursor)
    }
  }

  final case class Sum(computer: Computer) extends Operation {
    override val opCode: Int = 1
    override val op: (Int, Int) => Int = (_ + _)
  }

  final case class Multiplication(computer: Computer) extends Operation {
    override val opCode: Int = 2
    override val op: (Int, Int) => Int = (_ * _)
  }

  final case class Halt(computer: Computer) extends Instruction {
    override val opCode: Int = 99

    def result: Computer = computer
  }


  def run: IO[String] = {
    // TODO parellize
    // TODO check https://typelevel.org/cats-mtl/getting-started.html
    // TODO check for out of index
    // TODO probably only the loading of the input needs IO as long as the rest have the exception controlled
    for {
      lines <- loadResourceFile(INPUT).use(getLines)
      computer <- IO(Computer(lines.head.split(",").map(_.toInt).toVector))
      result <- IO(execute(computer.initialize(12, 2)))
//      computer <- IO(Computer(Vector(2,4,4,5,99,0)))
//      result <- IO(execute(computer))
      combination <- IO(findInitValues(computer, EXPECTED))
    } yield (result, combination).show
  }

  // A note:
  // Probably using State is an overkill, and using monad transformers even more, but, just for testing purposes
  def go: ComputerState[Result[Int]] =
    (for {
      inst <- EitherT(State.inspect(instruction))
      outInt <- EitherT[ComputerState, String, Int](
        inst match {
          case h: Halt => State.inspect(_ => Right(h.result(0)))
          case op: Operation => for {
            _ <- State.set(op.result)
            res <- go
          } yield res
        })
    } yield outInt).value

  def execute(computer: Computer): Result[Int] = go.runA(computer).value


  def findInitValues(computer: Computer, expected: Int): Result[Int] = {
    // TODO clean a bit
    val combinations = Semigroupal[LazyList].product(LazyList.range(1, 100), LazyList.range(1, 100))
    val resultsStream = combinations.find{
      case (noun, verb) =>
        execute(computer.initialize(noun, verb)) match {
          case Right(x) => if (x == expected) true else false
          case Left(_) => true
        }
    }
    resultsStream match {
      case None => Left("Combination not found")
      case Some((noun, verb)) =>
        execute(computer.initialize(noun, verb)) match {
          case Right(_) => Right(100 * noun + verb)
          case x => x // Other error we return
        }
    }
  }


  def instruction(computer: Computer): Result[Instruction] = computer.value match {
    case 99 => Right(Halt(computer))
    case 1 => Right(Sum(computer))
    case 2 => Right(Multiplication(computer))
    case _ => Left(s"${computer.value} is not a valid operation code")
  }

}
