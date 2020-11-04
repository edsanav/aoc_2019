package exercises

import cats.effect._
import auxiliar._
import algebra._

import cats.implicits._

object Main extends IOApp {

  val VALID_EXERCISES = List("2")

  def runModule(id:String):IO[Result[String]] = {
    // TODO this fails on missing file, maybe come up with an alternative?
    val inputLines = loadResourceFile(s"inputs/day${id}.csv").use(getLines)
    val runModule: List[String] => Result[String] =  {
      (x:List[String]) =>
      id match {
        case "1" => ex1.run(x).map(_.show)
        case "2" => ex2.run(x).map(_.show)
        case "3" => ex3.run(x).map(_.show)
      }
    }
    inputLines.map(runModule)
  }

  override def run(args: List[String]): IO[ExitCode] =
    args.headOption match {
        case None => IO(System.err.println("Usage: run <week number>")).as(ExitCode(2))
        case Some(module) => runModule(module).map{
          case Right(result) => println(s"Result is $result")
          case Left(error) => println(s"An error ocurred $error")
        }.as(ExitCode.Success)
    }

}