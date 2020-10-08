package exercises

import cats.effect._

object Main extends IOApp {

  def runModule(id:String):IO[Int] = id match {
    case "1" => ex1.run
    case "2" => ex2.run
    case _ => IO.raiseError(new IllegalArgumentException("Invalid week number"))
  }

  override def run(args: List[String]): IO[ExitCode] =
    args.headOption match {
        case None => IO(System.err.println("Usage: run <week number>")).as(ExitCode(2))
        case Some(module) => runModule(module).map(out => println(s"Result is $out")).as(ExitCode.Success)
    }

}