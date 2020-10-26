package exercises

import cats.data.{EitherT, State}
import cats.instances.either._
import exercises.auxiliar.loadResourceFile
import auxiliar._
import cats.Semigroupal
import cats.effect.IO
import cats.syntax.show._

object ex2 {

  val INPUT: String = "inputs/day2.csv"
  val EXPECTED = 19690720

  type ComputerState[A] = State[Computer, A]
  type Result[A] = Either[String, A] //TODO use proper errors not just strings
  type ComputerResult[A] = Either[ComputerError, A]


  sealed abstract class Error extends Exception

  // Computer related errors
  sealed abstract class ComputerError(message:String) extends Error
  case class MemoryError[A](v:Vector[A], pos:Int) extends ComputerError(message=s"Unable to access posicion $pos of vector $v")
  case class IllegalOperationError(op:Operation) extends ComputerError(message=s"Unable to perform $op")



  final case class Computer(mem: Vector[Int], cursor: Int = 0) {
    def updated(pos: Int, newVal: Int, newCursor:Int = cursor): Result[Computer] = {
      if (pos >= mem.size) Left(s"Invalid memory position $pos for computer of ${mem.size} size")
      else Right(Computer(mem.updated(pos, newVal), newCursor))
    }

    def initialize(noun: Int, verb: Int): Result[Computer] = updated(1, noun).flatMap(_.updated(2, verb))

    def value: Result[Int] = this.apply(cursor)

    def apply(pos:Int):Result[Int] =
      if (pos >= mem.size) Left(s"Invalid memory position $pos for computer of ${mem.size} size")
      else Right(mem(pos))
  }

  sealed abstract class Instruction {
    val opCode: Int
    def result: Result[Computer]
  }

  trait Operation extends Instruction {
    val computer: Computer
    val op: (Int, Int) => Int

    def result: Result[Computer] = {
      for {
        target <- computer(computer.cursor + 3)
        idx <- computer(computer.cursor + 1)
        idy <- computer(computer.cursor + 2)
        x <- computer(idx)
        y <- computer(idy)
        result <- computer.updated(target, op(x,y), computer.cursor + 4)
      } yield result
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

    def result: Result[Computer] = Right(computer)
  }


  def run: IO[String] = {
    // TODO parellize
    // TODO check https://typelevel.org/cats-mtl/getting-started.html
    // TODO check for out of index
    // TODO probably only the loading of the input needs IO as long as the rest have the exception controlled
    for {
      lines <- loadResourceFile(INPUT).use(getLines)
      computer <- IO(Computer(lines.head.split(",").map(_.toInt).toVector))
      result <- IO(computer.initialize(12, 2).flatMap(execute))
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
          case h: Halt => State.inspect(_ => h.result.flatMap(comp => comp(0)))
          case op: Operation => {
            val opRes = op.result
            opRes match {
              case Right(comp) => for {
                    _ <- State.set[Computer](comp)
                    res <- go //FIXME
                  } yield res
              case Left(l) => State.pure(Left(l))
            }
          }
        }
      )
    } yield outInt).value

  def execute(computer: Computer): Result[Int] = go.runA(computer).value


  def findInitValues(computer: Computer, expected: Int): Result[Int] = {
    // TODO clean a bit
    val combinations = Semigroupal[LazyList].product(LazyList.range(1, 100), LazyList.range(1, 100))
    val resultsStream = combinations.find{
      case (noun, verb) =>
        computer.initialize(noun, verb).flatMap(execute) match {
          case Right(x) => if (x == expected) true else false
          case Left(_) => true
        }
    }
    resultsStream match {
      case None => Left("Combination not found")
      case Some((noun, verb)) =>
        computer.initialize(noun, verb).flatMap(execute) match {
          case Right(_) => Right(100 * noun + verb)
          case x => x // Other error we return
        }
    }
  }


  def instruction(computer: Computer): Result[Instruction] = computer.value match {
    case Right(99) => Right(Halt(computer))
    case Right(1) => Right(Sum(computer))
    case Right(2) => Right(Multiplication(computer))
    case _ => Left(s"${computer.value} is not a valid operation code")
  }

}
