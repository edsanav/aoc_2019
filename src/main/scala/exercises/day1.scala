package exercises


import scala.io.Source

object day1 extends App {

  def fuelFormula(w: Int): Int = (w / 3) - 2

  def computeFuel(weight: Int): Int = {
    def go(w:Int, total:Int):Int = {
      val next_fuel = fuelFormula(w)
      if (next_fuel <=0 ) total
      else go(next_fuel, total+next_fuel)
    }
    go(weight, 0)
  }

  def readFile(file:String):List[Int] = {
    Source.fromResource(file).getLines.map(_.toInt).toList
  }

  def totalFuel(weights:List[Int]):Int = {
    weights.map(computeFuel).sum
  }

  def run(input_file:String):Int =  totalFuel(readFile(input_file))

  val result = run(args.headOption.getOrElse("inputs/day1.csv"))

  println(s"Day1: $result")

}

