package exercises

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object IntcodeComputer {
  //TODO refactor?

  type Result = Queue[Int] => (Vector[Int], Queue[Int]) // State?
  type ReadParam = Vector[Int] => Int

  def positional(p:Int): ReadParam = (v:Vector[Int]) => v(p)
  def immediate(p:Int): ReadParam = (_:Vector[Int]) => p

  sealed trait Operation

  trait Action extends Operation {
    def execute: Queue[Int] => (Vector[Int], Queue[Int])// probably can be done better than this
    def nextCursor(cursor:Int): Int
  }

  case class Sum(v:Vector[Int], p1: ReadParam, p2: ReadParam, result: Int) extends Action {
    def execute:Result = (o:Queue[Int]) => (v updated(result, p1(v) + p2(v)), o)
    def nextCursor(cursor:Int):Int = cursor + 4
  }

  case class Multiplication(v:Vector[Int], p1: ReadParam, p2: ReadParam, result: Int) extends Action {
    def execute:Result = (o:Queue[Int]) => (v updated(result, p1(v) * p2(v)), o)
    def nextCursor(cursor:Int):Int = cursor + 4
  }

  case class InputAction[A](v:Vector[Int], result:Int)(intFromInput:A=>(Int,A)) extends Action {
    def execute:Result = (o:Queue[Int]) =>  {
      val valFromIn = intFromInput
      (v updated(result, intFromInput), o)
    }
    def nextCursor(cursor:Int):Int = cursor + 2
  }

  case class OutputAction(v:Vector[Int], p1:ReadParam) extends Action {
    def execute:Result = (o:Queue[Int]) =>  {
      println(s"OUTPUT: ${p1(v)}")
      (v, o :+ p1(v))
    }
    def nextCursor(cursor:Int):Int = cursor + 2
  }

  case class JumpIfTrue(v:Vector[Int], p1:ReadParam, p2:ReadParam) extends Action {
    def execute:Result = (o:Queue[Int]) =>  (v,o)
    def nextCursor(cursor:Int):Int = {
      if (p1(v)!=0) p2(v)
      else cursor + 3
    }
  }

  case class JumpIfFalse(v:Vector[Int], p1:ReadParam, p2:ReadParam) extends Action {
    def execute:Result = (o:Queue[Int]) =>  (v,o)
    def nextCursor(cursor:Int):Int = {
      if (p1(v)==0) p2(v)
      else cursor + 3
    }
  }

  case class LessThan(v:Vector[Int], p1:ReadParam, p2:ReadParam, result:Int) extends Action {
    def execute:Result = (o:Queue[Int]) => {
      val toStore = if (p1(v) < p2(v)) 1 else 0
      (v updated(result, toStore), o)
    }
    def nextCursor(cursor:Int):Int = cursor + 4
  }

  case class Equals(v:Vector[Int], p1:ReadParam, p2:ReadParam, result:Int) extends Action {
    def execute:Result = (o:Queue[Int]) => {
      val toStore = if (p1(v) == p2(v)) 1 else 0
      (v updated(result, toStore),o)
    }
    def nextCursor(cursor:Int):Int = cursor + 4
  }

  case object End extends Operation


  def getReadParam(v:Vector[Int], cursor:Int)(opCode:Int)(opCursor: Int):ReadParam = {
    val mode = (opCode / 100) / math.pow(10, opCursor-1).toInt % 10
    if (mode == 1) immediate(v(cursor+opCursor))
    else positional(v(cursor+opCursor))
  }

  def translate[A](in:A => (Int,A))(v:Vector[Int], cursor:Int):Operation = {
    val opCode = v(cursor)
    val op:Int = opCode % 100
    def rpF:Int => ReadParam = getReadParam(v, cursor)(opCode) // partially applied f to get ReadParam from opCursor
    if (op == 1) Sum(v, rpF(1),  rpF(2), v(cursor+3))
    else if (op == 2) Multiplication(v, rpF(1),  rpF(2), v(cursor+3))
    else if (op == 3) InputAction(v, v(cursor+1))(in)
    else if (op == 4) OutputAction(v, rpF(1))
    else if (op == 5) JumpIfTrue(v, rpF(1), rpF(2))
    else if (op == 6) JumpIfFalse(v, rpF(1), rpF(2))
    else if (op == 7) LessThan(v, rpF(1), rpF(2), v(cursor+3))
    else if (op == 8) Equals(v, rpF(1), rpF(2), v(cursor+3))
    else if (op == 99) End
    else throw new IllegalArgumentException("Invalid operation")
  }



  @tailrec
  def operation[A](in:A => (Int,A))(output:Queue[Int])(v:Vector[Int], cursor:Int):(Vector[Int], Int, Queue[Int]) = {
    translate(in:A => (Int,A))(v, cursor) match {
      case End => (v, cursor, output)
      case op:Action => {
        val (newV, out2) = op.execute(output)
        operation(in)(out2)(newV, op.nextCursor(cursor))
      }
    }
  }

  def readUserInput():Int = {
    //This, as the readInput is not very functional. Consider using some libraries (Cats? ZIO?) and cleaning it a bit
    println("PLEASE, PROVIDE AN INPUT VALUE AND PRESS ENTER")
    scala.io.StdIn.readInt()
  }

  def operationFromUserInput:(Vector[Int], Int) => (Vector[Int], Int) = {
    (v:Vector[Int],c:Int) => {
      val (v2, c2, o) = operation(readUserInput())(Queue[Int]())(v, c)
      (v2,c2)
    }
  }


}
