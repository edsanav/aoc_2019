package exercises

import cats.effect._


object ex1 {

  def run: IO[ExitCode] = {
    for {
      _ <- IO(println("Yey exercise1"))
    } yield ExitCode.Success
  }
}
