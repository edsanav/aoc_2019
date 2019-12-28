import scala.io.Source
import scala.reflect.ClassTag

package object exercises {

  def splitToVector[T:ClassTag](as:String=>T)(csv:String, sep:String=","):Vector[T] =
    csv.split(sep).map(as(_)).toVector

  def readVector[T:ClassTag](as:String=>T)(file:String, sep:String=","):Vector[T] = {
    val lines = Source.fromResource(file).getLines()
    if(lines.hasNext) splitToVector(as)(lines.next, sep=sep) else Vector[T]()
  }

  def readIntsVector(file:String, sep:String=","):Vector[Int] = readVector((x:String) => x.toInt)(file, sep)
  def readBigIntsVector(file:String, sep:String=","):Vector[BigInt]  = readVector((x:String) => BigInt(x))(file, sep)
}
