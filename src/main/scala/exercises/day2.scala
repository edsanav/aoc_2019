package exercises

import exercises.day2.Multiplication

import scala.io.Source


object day2 {

  def readInput(file:String):Vector[Int] = {
    val lines = Source.fromResource(file).getLines()
    if(lines.hasNext) lines.next.split(",").map(_.toInt).toVector else Vector[Int]()
  }

  sealed trait Operation

  trait Action extends Operation {
    def execute(v:Vector[Int]):Vector[Int]
  }

  case class Sum(p1: Int, p2: Int, result: Int) extends Action {
    def execute(v:Vector[Int]):Vector[Int]  = v updated(result, v(p1) + v(p2))
  }

  case class Multiplication(p1: Int, p2: Int, result: Int) extends Action {
    def execute(v:Vector[Int]):Vector[Int]  = v updated(result, v(p1) * v(p2))
  }

  case object End extends Operation

  def translate(v:Vector[Int], cursor:Int):Operation = {
    val opCode = v(cursor)
    if (opCode == 1) Sum(v(cursor+1), v(cursor+2), v(cursor+3))
    else if (opCode == 2) Multiplication(v(cursor+1), v(cursor+2), v(cursor+3))
    else if (opCode == 99) End
    else throw new IllegalArgumentException("Invalid operation")
  }

  def operation(v:Vector[Int], cursor:Int):(Vector[Int], Int) = {
    translate(v, cursor) match {
      case End => (v, cursor)
      case op:Action => operation(op.execute(v), cursor + 4)
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

  def run() = {
    val rawV = readInput("inputs/day2.csv")
    //compute(12, 2, rawV)
    val (noun, verb, _) = findCombination(rawV).find( x => x._3 == 19690720).get
    (noun*100 + verb)
  }


}
