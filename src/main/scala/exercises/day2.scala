package exercises

import scala.annotation.tailrec
import scala.io.Source


object day2 extends App{

  def readInput(file:String):Vector[Int] = {
    val lines = Source.fromResource(file).getLines()
    if(lines.hasNext) lines.next.split(",").map(_.toInt).toVector else Vector[Int]()
  }

  def readUserInput():Int = {
    //This, as the readInput is not very functional. Consider using some libraries (Cats? ZIO?) and cleaning it a bit
    println("PLEASE, PROVIDE AN INPUT VALUE AND PRESS ENTER")
    scala.io.StdIn.readInt()
  }


  type ReadParam = Vector[Int] => Int

  def positional(p:Int): ReadParam = (v:Vector[Int]) => v(p)
  def immediate(p:Int): ReadParam = (_:Vector[Int]) => p

  sealed trait Operation

  trait Action extends Operation {
    def execute:Vector[Int]
    def nextCursor(cursor:Int): Int
  }

  case class Sum(v:Vector[Int], p1: ReadParam, p2: ReadParam, result: Int) extends Action {
    def execute:Vector[Int]  = v updated(result, p1(v) + p2(v))
    def nextCursor(cursor:Int):Int = cursor + 4
  }

  case class Multiplication(v:Vector[Int], p1: ReadParam, p2: ReadParam, result: Int) extends Action {
    def execute:Vector[Int]  = v updated(result, p1(v) * p2(v))
    def nextCursor(cursor:Int):Int = cursor + 4
  }

  case class InputAction(v:Vector[Int], result:Int) extends Action {
    def execute:Vector[Int]   = {
      v updated(result, readUserInput())
    }
    def nextCursor(cursor:Int):Int = cursor + 2
  }

  case class OutputAction(v:Vector[Int], p1:ReadParam) extends Action {
    def execute:Vector[Int]  = {
      println(s"OUTPUT: ${p1(v)}")
      v
    }
    def nextCursor(cursor:Int):Int = cursor + 2
  }

  case class JumpIfTrue(v:Vector[Int], p1:ReadParam, p2:ReadParam) extends Action {
    def execute:Vector[Int]  = ???
    def nextCursor(cursor:Int):Int = cursor + 3 //CAREFUL!
  }

  case class JumpIfFalse(v:Vector[Int], p1:ReadParam, p2:ReadParam) extends Action {
    def execute:Vector[Int]  = ???
    def nextCursor(cursor:Int):Int = cursor + 2
  }

  case class LessThan(v:Vector[Int], p1:ReadParam, p2:ReadParam) extends Action {
    def execute:Vector[Int]  = ???
    def nextCursor(cursor:Int):Int = cursor + 2
  }

  case class Equals(v:Vector[Int], p1:ReadParam, p2:ReadParam) extends Action {
    def execute:Vector[Int]  = ???
    def nextCursor(cursor:Int):Int = cursor + 2
  }

  case object End extends Operation


  def getReadParam(v:Vector[Int], cursor:Int)(opCode:Int)(opCursor: Int):ReadParam = {
    val mode = (opCode / 100) / math.pow(10, opCursor-1).toInt % 10
    if (mode == 1) immediate(v(cursor+opCursor))
    else positional(v(cursor+opCursor))
  }

  def translate(v:Vector[Int], cursor:Int):Operation = {
    val opCode = v(cursor)
    val op:Int = opCode % 100
    def rpF:Int => ReadParam = getReadParam(v, cursor)(opCode) // partially applied f to get ReadParam from opCursor
    if (op == 1) Sum(v, rpF(1),  rpF(2), v(cursor+3))
    else if (op == 2) Multiplication(v, rpF(1),  rpF(2), v(cursor+3))
    else if (op == 3) InputAction(v, v(cursor+1))
    else if (op == 4) OutputAction(v, rpF(1))
    else if (op == 5) OutputAction(v, rpF(1))
    else if (op == 99) End
    else throw new IllegalArgumentException("Invalid operation")
  }

  @tailrec
  def operation(v:Vector[Int], cursor:Int):(Vector[Int], Int) = {
    translate(v, cursor) match {
      case End => (v, cursor)
      case op:Action => operation(op.execute, op.nextCursor(cursor))
    }
  }

  def compute(noun:Int, verb:Int, rawV:Vector[Int]):Int = {
    val startV = rawV updated(1, noun) updated (2, verb)
    val (finalV, _) = operation(startV, 0)
    finalV(0)
  }

  def findCombination(v:Vector[Int]):LazyList[(Int,Int,Int)] = {
    for {
      x <- LazyList range(0, 99)
      y <- LazyList range(0, 99)
    } yield (x,y, compute(x, y, v))
  }

  def run(input_file:String):(Int,Int) = {
    val rawV = readInput(input_file)
    val part1 = compute(12, 2, rawV)
    val (noun, verb, _) = findCombination(rawV).find( x => x._3 == 19690720).get
    val part2 = (noun*100 + verb)
    (part1, part2)
  }

  val (result1, result2) = run(args.headOption.getOrElse("inputs/day2.csv"))
  println(s"Day2-1: $result1")
  println(s"Day2-2: $result2")

}
