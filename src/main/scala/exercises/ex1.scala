package exercises

import cats.effect._
import cats.Foldable
import cats.instances.list._
import cats.syntax.show._
import auxiliar._

import scala.annotation.tailrec

object ex1 {

  val INPUT:String = "inputs/day1.csv"

  def run: IO[String] = {
    for {
      lines <- loadResourceFile(INPUT).use(getLines)
      values <- IO(lines.map(_.toInt))
      result <-IO(sumFuel(values).show)
    } yield result
  }

  def getFuel(mass:Int):Int = (mass.toDouble / 3 ).floor.toInt - 2

  def sumFuel[F[_]: Foldable](masses:List[Int]):Int =
    masses.foldLeft(0){case (z, b) =>
      val fuel = getFuel(b)
      val extra = computeExtra(fuel)
      z + fuel + extra
    }

  def computeExtra(fuelMass:Int):Int = {
    @tailrec
    def go(remaining: Int, accum:Int):Int = {
      val fuelForFuel = getFuel(remaining)
      if (fuelForFuel > 0) go(fuelForFuel, accum+fuelForFuel)
      else accum
    }
    go(fuelMass, 0)
  }


}
