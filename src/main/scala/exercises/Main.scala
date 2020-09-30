package exercises

import cats.effect._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _  <- if(args.length < 1) IO.raiseError(new IllegalArgumentException("Need week number to execute"))
            else IO.unit
      _ <- args.head match {
            case "1" => ex1.run
            case _ => IO.raiseError(new IllegalArgumentException("Invalid week number"))
      }
    } yield ExitCode.Success
}