package exercises

import day1._

import scala.io.Source


object Main extends App {


  val input1:List[Int] = Source.fromResource("inputs/day1.csv").getLines.map(_.toInt).toList

  println(compute_fuel(total_fuel(input1)))
}
