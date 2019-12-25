package exercises

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object IntcodeComputer {
  //TODO refactor?

  case class Computer(v:Vector[Int], cursor:Int)

  type Result = Queue[Int] => (Vector[Int], Queue[Int]) // State?
  type ReadParam = Vector[Int] => Int

  def positional(p:Int): ReadParam = (v:Vector[Int]) => v(p)
  def immediate(p:Int): ReadParam = (_:Vector[Int]) => p

  sealed trait Operation

  trait Action extends Operation {
    def execute: Computer => Computer// probably can be done better than this
  }

  trait ChannelAction[A] extends Operation {
    def execute: (Computer, A) => (Computer, A)
  }

  case class Sum(p1: ReadParam, p2: ReadParam, result: Int) extends Action {
    def execute:Computer => Computer =
      (c: Computer) => Computer(c.v.updated(result, p1(c.v) + p2(c.v)), c.cursor+4)
  }

  case class Multiplication(p1: ReadParam, p2: ReadParam, result: Int) extends Action {
    def execute:Computer => Computer =
      (c: Computer) => Computer(c.v.updated(result, p1(c.v) * p2(c.v)), c.cursor+4)
  }

  case class ManualInputAction(result:Int) extends ChannelAction[Unit] {
    def execute:(Computer, Unit) => (Computer, Unit) =
      (c: Computer, Unit) => (Computer(c.v.updated(result, readUserInput()), c.cursor+2), Unit)
  }

  case class QueueInputAction(result:Int) extends ChannelAction[Queue[Int]] {

    def execute:(Computer, Queue[Int]) => (Computer, Queue[Int]) =
      (c: Computer, q:Queue[Int]) => {
        val (intval, newQ) = q.dequeue
        (Computer(c.v.updated(result, intval), c.cursor+2), newQ)
      }

  }

  case class QueueOutputAction(p1:ReadParam) extends ChannelAction[Queue[Int]] {
    def execute:(Computer, Queue[Int]) => (Computer, Queue[Int]) =
      (c: Computer, q:Queue[Int]) => (Computer(c.v, c.cursor+2), q:+ p1(v))

  }

  case class JumpIfTrue(p1:ReadParam, p2:ReadParam) extends Action {
    def execute:Computer => Computer =
      (c: Computer) => {
        val nextCursor = if (p1(c.v)!=0) p2(c.v) else c.cursor + 3
        Computer(c.v, nextCursor)
      }
  }

  case class JumpIfFalse(p1:ReadParam, p2:ReadParam) extends Action {
    def execute:Computer => Computer =
      (c: Computer) => {
        val nextCursor = if (p1(c.v)==0) p2(c.v) else c.cursor + 3
        Computer(c.v, nextCursor)
      }
  }


  case class LessThan(p1:ReadParam, p2:ReadParam, result:Int) extends Action {
    def execute:Computer => Computer =
      (c: Computer) => {
        val toStore = if (p1(c.v) < p2(c.v)) 1 else 0
        Computer(c.v.updated(result, toStore), c.cursor+4)
      }
  }

  case class Equals(p1:ReadParam, p2:ReadParam, result:Int) extends Action {
    def execute:Computer => Computer =
      (c: Computer) => {
        val toStore = if (p1(c.v) == p2(c.v)) 1 else 0
        Computer(c.v.updated(result, toStore), c.cursor+4)
      }
  }

  case object End extends Operation


  def getReadParam(v:Vector[Int], cursor:Int)(opCode:Int)(opCursor: Int):ReadParam = {
    val mode = (opCode / 100) / math.pow(10, opCursor-1).toInt % 10
    if (mode == 1) immediate(v(cursor+opCursor))
    else positional(v(cursor+opCursor))
  }

  def translate[I,O](intActionFactory:Int => ChannelAction[I])
                    (outActionFactory:ReadParam => ChannelAction[O])
                    (c:Computer):Operation = {
    val opCode = c.v(c.cursor)
    val op:Int = opCode % 100
    def rpF:Int => ReadParam = getReadParam(c.v, c.cursor)(opCode) // partially applied f to get ReadParam from opCursor
    if (op == 1) Sum(rpF(1),  rpF(2), c.v(c.cursor+3))
    else if (op == 2) Multiplication(rpF(1),  rpF(2), c.v(c.cursor+3))
    else if (op == 3) intActionFactory(c.v())
    else if (op == 4) outActionFactory(rpF(1))
    else if (op == 5) JumpIfTrue(rpF(1), rpF(2))
    else if (op == 6) JumpIfFalse(rpF(1), rpF(2))
    else if (op == 7) LessThan(rpF(1), rpF(2), c.v(c.cursor+3))
    else if (op == 8) Equals(rpF(1), rpF(2), c.v(c.cursor+3))
    else if (op == 99) End
    else throw new IllegalArgumentException("Invalid operation")
  }



  @tailrec
  def operation[I,O](in:I)(output:O)(v:Vector[Int], cursor:Int):(Vector[Int], Int, Queue[Int]) = {
    translate(in:A => (Int,A))(v, cursor) match {
      case End => (v, cursor, output)
      case op:ChannelAction =>
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
