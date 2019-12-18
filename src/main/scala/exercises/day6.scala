package exercises
import scala.collection.immutable.HashMap
import scala.io.Source
import scala.collection.immutable.Queue

object day6 extends App{

  case class Planet(code:String, orbiters:HashMap[String, Planet], orbited:Queue[String]){
    val checkSum:Int = orbiters.values.foldLeft(orbited.size)((accum, x)=> accum + x.checkSum)

    def differentOrbits(p2:Planet):(Queue[String],Queue[String]) = {
      val (common, onp1) = orbited.partition(s => p2.orbited.contains(s))
      val onp2 = p2.orbited.dropWhile(common.contains)
      (onp1, onp2)
    }

    def get(c:String):Option[Planet] = {
      orbiters.get(c) match {
        case Some(x) => Some(x)
        case None => orbiters.values.flatMap(x => x.get(c)).find{p:Planet => true}
      }
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


  def toPlanet(code:String, orbitMap:HashMap[String, Set[String]], orbited:Queue[String]):Planet = {
    Planet(
      code,
      orbitMap(code).foldLeft(HashMap[String, Planet]()) { (total, x) =>
        val planet = toPlanet(x, orbitMap, orbited :+ code)
        total + (planet.code -> planet)
      },
      orbited
    )
  }

  def run(input_file:String):(Int,Int) = {
    val orbitMap = organizeOrbits(Source.fromResource(input_file).getLines)
    val wholeSystem = toPlanet("COM", orbitMap, Queue[String]())
    val you = wholeSystem.get("YOU").get
    val santa = wholeSystem.get("SAN").get
    val (yourOrbits,santaOrbits) =  you.differentOrbits(santa)
    (wholeSystem.checkSum, yourOrbits.size+santaOrbits.size)
  }



  val (result1, result2) = run(args.headOption.getOrElse("inputs/day6.csv"))
  println(s"Day6-1: $result1")
  println(s"Day6-2: $result2")


}
