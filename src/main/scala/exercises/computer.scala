package exercises

import cats.data.{EitherT, State}
import exercises.algebra.{ComputerError, Result}

object computer {

  type ComputerState[A] = State[Computer, A]
  type ComputerResult[A] = Either[ComputerError, A]

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

  def instruction(computer: Computer): Result[Instruction] = computer.value match {
    case Right(99) => Right(Halt(computer))
    case Right(1) => Right(Sum(computer))
    case Right(2) => Right(Multiplication(computer))
    case _ => Left(s"${computer.value} is not a valid operation code")
  }

  // A note:
  // Probably using State is an overkill, and using monad transformers even more, but, just for testing purposes
  def go: ComputerState[Result[Int]] = {
    (for {
      inst <- EitherT(State.inspect(instruction))
      outInt <- EitherT[ComputerState, String, Int](
        inst match {
          case h: Halt => State.inspect(_ => h.result.flatMap(comp => comp(0)))
          case op: Operation => op.result match {
            case Right(comp) => for {
              _ <- State.set[Computer](comp)
              res <- go //
            } yield res
            case Left(l) => State.pure(Left(l))
          }
        }
      )
    } yield outInt).value
  }

  def execute(computer: Computer): Result[Int] = go.runA(computer).value

}
