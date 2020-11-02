package exercises

import cats.effect._
import auxiliar._

import cats.implicits._

object Main extends IOApp {

  def runModule(id:String):IO[String] = {
    val input = s"inputs/day${id}.csv"
    val inputLines = loadResourceFile(input).use(getLines)
    // TODO change List[Sting] => Result[String] and avoid IO dependency in submodules
    val runModule: List[String] => IO[String] =  { (x:List[String]) =>
      id match {
        case "1" => ex1.run(x).map(_.show)
        case "2" => ex2.run(x).map(_.show)
        case _ =>  IO.raiseError(new IllegalArgumentException("Invalid week number"))
      }
    }
    inputLines.flatMap(runModule)
  }

  override def run(args: List[String]): IO[ExitCode] =
    args.headOption match {
        case None => IO(System.err.println("Usage: run <week number>")).as(ExitCode(2))
        case Some(module) => runModule(module).map(out => println(s"Result is $out")).as(ExitCode.Success)
    }

}