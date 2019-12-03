package exercises

import exercises.day2.Multiplication

import scala.io.Source


object day2 {

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      flatMap((a: A) => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap((a: A) => sb.map(b => f(a, b)))


    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State((s: S) => {
        val (a: A, s1: S) = run(s)
        f(a).run(s1)
      }
      )
    }
  }

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def modifyS[S](f: S => S): State[S, Unit] = for {
    s <- getS // Gets the current state and assigns it to `s`.
    _ <- setS(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def getS[S]: State[S, S] = State(s => (s, s))

  def setS[S](s: S): State[S, Unit] = State(_ => ((), s))

  /* More general concepts */

  def readInput(file:String):Vector[Int] = {
    val lines = Source.fromResource(file).getLines()
    if(lines.hasNext) lines.next.split(",").map(_.toInt).toVector else Vector[Int]()
  }

  sealed trait Operation

  case class Sum(p1: Int, p2: Int, result: Int) extends Operation {
    def execute(v:Vector[Int]):Vector[Int]  = v updated(result, v(p1) + v(p2))
  }

  case class Multiplication(p1: Int, p2: Int, result: Int) extends Operation {
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
      case op:Sum => operation(op.execute(v), cursor + 4)
      case op:Multiplication =>operation(op.execute(v), cursor + 4)
    }
  }

  def compute(noun:Int, verb:Int, rawV:Vector[Int]):Int = {
    val startV = rawV updated(1, noun) updated (2, verb)
    val (finalV, _) = operation(startV, 0)
    finalV(0)
  }

  def findCombination(v:Vector[Int]) = {
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
