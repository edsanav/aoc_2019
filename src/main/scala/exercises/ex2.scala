package exercises

import cats.instances.either._
import cats.Semigroupal
import cats.effect.{ContextShift, IO}
import cats.syntax.all._
import exercises.computer.{Computer, Result, execute}

object ex2 {

  val EXPECTED = 19690720



  def run(lines:List[String])(implicit cs:ContextShift[IO]): IO[Result[(Int,Int)]] = {
    for {
      input <- IO(lines.head.split(",").map(_.toInt).toVector)
      result = Computer(input).initialize(12, 2).flatMap(execute)
      combination = findInitValues(Computer(input), EXPECTED)
      finalRes <- IO((result, combination).parMapN((_,_)))
    } yield finalRes
  }


  def findInitValues(computer: Computer, expected: Int): Result[Int] = {
    // Stream of combination
    val combinations = Semigroupal[LazyList].product(LazyList.range(1, 100), LazyList.range(1, 100))
    // Results are not calculated until required (lazy list is a stream)
    val inputResults = combinations.map {
      case (noun, verb) => ((noun, verb), computer.initialize(noun, verb).flatMap(execute))
    }
    // We search for the first hit or the first error of the stream
    val hitOrError = inputResults.find{case (_, result) => result match {
      case Right(x) => expected == x
      case Left(_) => true
      }
    }
    // Verify if found value is a hit or an error and act accordingly
    hitOrError match {
      case Some(((noun,verb), result)) => result match {
        case Right(_) => Right(100 * noun + verb) // With hit, noun and verb are used to compute value
        case left => left // Error is returned directly
      }
      case None => Left("Combination not found")
    }
  }


}
