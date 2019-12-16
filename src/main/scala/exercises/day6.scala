package exercises
import scala.collection.immutable.HashMap
import scala.io.Source

object day6 extends App{

  case class Planet(code:String, orbiters:Set[Planet], distance:Int=0){


    val checkSum:Int = orbiters.foldLeft(distance)((accum, x)=> accum + x.checkSum)
  }

  def splitOrbit(orbitStr:String):(String, String) = {
    val splitted =orbitStr.split("\\)")
    (splitted.head, splitted(1))
  }

  def organizeOrbits(orbitsStrings:Iterator[String]):HashMap[String, Set[String]] = {
    orbitsStrings.foldLeft(HashMap[String, Set[String ]]()){(accum:HashMap[String, Set[String]], a:String) =>
      val (centre, orbiter) = splitOrbit(a)
      // Initialize orbiter
      val withOrbiter = accum + (orbiter -> (accum.getOrElse(orbiter, Set[String]())))
      // Adding centre with the orbiter
      withOrbiter + (centre -> (withOrbiter.getOrElse(centre, Set[String]()) + orbiter))
    }
  }


  def toPlanet(code:String, orbitMap:HashMap[String, Set[String]], distance:Int=0):Planet = {
    Planet(code, orbitMap(code).foldLeft(Set[Planet]())((total, x)=> total + toPlanet(x, orbitMap, distance+1)), distance)
  }

  def run(input_file:String):(Int,Int) = {
    val orbitMap = organizeOrbits(Source.fromResource(input_file).getLines)
    val wholeSystem = toPlanet("COM", orbitMap)
    (wholeSystem.checkSum, 0)
  }



  val (result1, result2) = run(args.headOption.getOrElse("inputs/day6.csv"))
  println(s"Day5-1: $result1")
  println(s"Day5-2: $result2")


}
