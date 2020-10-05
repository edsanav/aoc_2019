package exercises

import cats.effect._

import auxiliar._

object ex1 {

  val INPUT:String = "inputs/day1.csv"

  def run: IO[ExitCode] = {
    for {
      lines <- loadResourceFile(INPUT).use(getLines)
      values <- IO(lines.map(_.toInt))
      _ <- IO(println(values))
    } yield ExitCode.Success
  }


}
