package exercises

import cats.effect.{IO, Resource}

import scala.io.{BufferedSource, Source}

object auxiliar {


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
