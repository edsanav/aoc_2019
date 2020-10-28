package exercises

import cats.data.{EitherT, State}
import cats.{Applicative, Id, Monad, MonadError}
import cats.implicits._
import cats.mtl.{Raise, Stateful}

object computer {


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

  def instruction(computer: Computer): Result[Instruction] = computer.value match {
    case Right(99) => Right(Halt(computer))
    case Right(1) => Right(Sum(computer))
    case Right(2) => Right(Multiplication(computer))
    case _ => Left(s"${computer.value} is not a valid operation code")
  }



  def parseNumber[F[_]: Applicative](in: String)(implicit F: Raise[F, String]): F[Int] = {
    if (in.matches("-?[0-9]+")) in.toInt.pure[F]
    else F.raise(show"'$in' could not be parsed as a number")
  }

  def instruction2[F[+_]: Applicative](computer:Computer)(implicit F: Raise[F, String]): F[Instruction] = computer.value match {
    case Right(99) => Halt(computer).pure[F]
    case Right(1) => Sum(computer).pure[F]
    case Right(2) => Multiplication(computer).pure[F]
    case _ => F.raise(s"${computer.value} is not a valid operation code")
  }


  // A note:
  // Probably using State is an overkill, and using monad transformers even more, but, just for testing purposes
  def go: EitherT[ComputerState, String, Int] = {
    for {
      inst <- EitherT(State.inspect(instruction))
      outInt <- EitherT[ComputerState, String, Int](
        inst match {
          case h: Halt => State.inspect(_ => h.result.flatMap(comp => comp(0)))
          case op: Operation => op.result match {
            case Right(comp) => for {
              _ <- State.set[Computer](comp)
              res <- go.value //
            } yield res
            case Left(l) => State.pure(Left(l))
          }
        }
      )
    } yield outInt
  }

  def go2[F[+_]:Applicative, G[_]:Monad](implicit S: Stateful[G, Computer], E: Raise[F, String]): G[F[Instruction]] = {
    (for {
      inst <- S.inspect(instruction2[F])
    } yield inst)
  }

  def execute(computer: Computer): Result[Int] = go.value.runA(computer).value


}
