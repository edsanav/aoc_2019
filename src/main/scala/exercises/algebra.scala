package exercises

import cats.data.State
import exercises.computer.{Computer, Operation}

object algebra {


  type Result[A] = Either[String, A] //TODO use proper errors not just strings



  sealed abstract class Error extends Exception

  // Computer related errors
  sealed abstract class ComputerError(message:String) extends Error
  case class MemoryError[A](v:Vector[A], pos:Int) extends ComputerError(message=s"Unable to access posicion $pos of vector $v")
  case class IllegalOperationError(op:Operation) extends ComputerError(message=s"Unable to perform $op")


}
