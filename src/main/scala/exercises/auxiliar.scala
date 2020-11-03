package exercises

import cats.effect.{IO, Resource}
import exercises.algebra.Result
import cats.syntax.traverse._


import scala.io.{BufferedSource, Source}

object auxiliar {

  def toInts(list:List[String]):Result[List[Int]] = list.map(x => (x,x.toIntOption)).traverse{
      case (_, Some(x)) => Right(x)
      case (invalid, None) => Left(s"Invalid input val $invalid")
  }


  def loadResourceFile(path:String): Resource[IO, BufferedSource] =
    Resource.make {
      IO(Source.fromResource(path))
    } { source =>
      IO(source.close()).handleErrorWith(_ => IO.unit)
    }

  def getLines(source:BufferedSource): IO[List[String]] = {
    for {
      lines <- IO(source.getLines())
    } yield lines.toList
  }


}
