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

  trait InputAction[A] extends Operation {
    def execute: (Computer, A) => (Computer, A)
  }

  trait OutputAction[A] extends Operation {
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

  case class ManualInputAction(result:Int) extends InputAction[Unit] {
    def execute:(Computer, Unit) => (Computer, Unit) =
      (c: Computer, Unit) => (Computer(c.v.updated(result, readUserInput()), c.cursor+2), Unit)
  }

  case class QueueInputAction(result:Int) extends InputAction[Queue[Int]] {

    def execute:(Computer, Queue[Int]) => (Computer, Queue[Int]) =
      (c: Computer, q:Queue[Int]) => {
        val (intval, newQ) = q.dequeue
        (Computer(c.v.updated(result, intval), c.cursor+2), newQ)
      }

  }

  case class ConsoleOutputAction(p1:ReadParam) extends OutputAction[Unit] {
    def execute:(Computer, Unit) => (Computer, Unit) =
      (c: Computer, Unit) => {
        println(s"CONSOLE OUTPUT:${p1(c.v)}")
        (Computer(c.v, c.cursor+2), Unit)
      }
  }

  case class QueueOutputAction(p1:ReadParam) extends OutputAction[Queue[Int]] {
    def execute:(Computer, Queue[Int]) => (Computer, Queue[Int]) =
      (c: Computer, q:Queue[Int]) => (Computer(c.v, c.cursor+2), q:+ p1(c.v))
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

  def translate[I,O](intActionFactory:Int => InputAction[I], outActionFactory:ReadParam => OutputAction[O])
                    (c:Computer):Operation = {
    val opCode = c.v(c.cursor)
    val op:Int = opCode % 100
    def rpF:Int => ReadParam = getReadParam(c.v, c.cursor)(opCode) // partially applied f to get ReadParam from opCursor
    if (op == 1) Sum(rpF(1),  rpF(2), c.v(c.cursor+3))
    else if (op == 2) Multiplication(rpF(1),  rpF(2), c.v(c.cursor+3))
    else if (op == 3) intActionFactory(c.v(c.cursor+1))
    else if (op == 4) outActionFactory(rpF(1))
    else if (op == 5) JumpIfTrue(rpF(1), rpF(2))
    else if (op == 6) JumpIfFalse(rpF(1), rpF(2))
    else if (op == 7) LessThan(rpF(1), rpF(2), c.v(c.cursor+3))
    else if (op == 8) Equals(rpF(1), rpF(2), c.v(c.cursor+3))
    else if (op == 99) End
    else throw new IllegalArgumentException("Invalid operation")
  }



  @tailrec
  def operation[I,O](intActionF:Int => InputAction[I],
                     outActionF:ReadParam => OutputAction[O])
                    (in:I)(out:O)(c:Computer):(Computer, O) = {
    translate(intActionF, outActionF)(c) match {
      case End => (c, out)
      case op:InputAction[I] =>
        val (newC, newIn) = op.execute(c,in)
        operation(intActionF, outActionF)(newIn)(out)(newC)
      case op:OutputAction[O] =>
        val (newC, newOut) = op.execute(c,out)
        operation(intActionF, outActionF)(in)(newOut)(newC)
      case op:Action => operation(intActionF, outActionF)(in)(out)(op.execute(c))

    }
  }

  def readUserInput():Int = {
    //This, as the readInput is not very functional. Consider using some libraries (Cats? ZIO?) and cleaning it a bit
    println("PLEASE, PROVIDE AN INPUT VALUE AND PRESS ENTER")
    scala.io.StdIn.readInt()
  }

  /** Convenience method to */
  def operationFromUserInput(c:Computer): Computer = {
    val (finalC, _) = operation(ManualInputAction.apply, ConsoleOutputAction.apply)(())(())(c)
    finalC
  }

  def operationsQueues(c:Computer)(in:Queue[Int]):(Computer, Queue[Int]) = {
    operation(QueueInputAction.apply, QueueOutputAction.apply)(in)(Queue[Int]())(c)
  }


}
