import scala.io.Source

package object exercises {

  def splitToVector(csv:String, sep:String=","):Vector[Int] = csv.split(sep).map(_.toInt).toVector

  def readVector(file:String, sep:String=","):Vector[Int] = {
    val lines = Source.fromResource(file).getLines()
    if(lines.hasNext) splitToVector(lines.next, sep=sep) else Vector[Int]()
  }

}
