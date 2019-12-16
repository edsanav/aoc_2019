package exercises
import scala.collection.immutable.HashMap
import scala.io.Source

object day6 extends App{

  case class Planet(code:String, orbiters:Set[Planet]){

    def orbits(step:Int):Int = orbiters.size * step
    val checkSum= {
      val toSum = orbiters.foldLeft(List((orbiters.size * 1, 1))((total, x) => List((orbiters.size, * total._2 +1)
    }
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


  def toPlanet(code:String, orbitMap:HashMap[String, Set[String]]):Planet = {
    Planet(code, orbitMap(code).foldLeft(Set[Planet]())((total, x)=> total + toPlanet(x, orbitMap)))
  }

  def run(input_file:String):(Int,Int) = {
    val orbitMap = organizeOrbits(Source.fromResource(input_file).getLines)
    val wholeSystem = toPlanet("COM", orbitMap)
    val in2 = Iterator("COM)B", "B)C", "C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L")
    val orbitMap2 = organizeOrbits(in2)
    val sample =  toPlanet("COM", orbitMap2)
    println(sample.checkSum)
    println(sample)

    (wholeSystem.checkSum, 0)
  }


  val (result1, result2) = run(args.headOption.getOrElse("inputs/day6.csv"))
  println(s"Day5-1: $result1")
  println(s"Day5-2: $result2")


}
