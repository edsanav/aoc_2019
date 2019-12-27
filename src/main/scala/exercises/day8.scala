package exercises

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.immutable.HashMap

object day8 extends App{

  case class Layer(pixels:Vector[Int]){
    lazy val counts:HashMap[Int, Int] = pixels.foldLeft(HashMap[Int, Int]())(
      (m, pixel) => m + (pixel -> (m.getOrElse(pixel, 0)+1))
    )
    lazy val rows:Vector[Vector[Int]] = pixels.grouped(Layer.width).toVector
  }

  object Layer{
    val width:Int = 25
    val height:Int = 6
    val flatLength:Int = width*height
  }

  @tailrec
  def getColor(ls:List[Int]):Int = ls match {
    case Nil => 2
    case x::_ if x !=2 => x
    case x::xs if x==2 => getColor(xs)
  }

  def getPixel(layers:List[Layer], position:Int):Int = getColor(layers.map(_.pixels(position)))

  def getStackedVector(layers:List[Layer]):Layer =
    Layer(layers.head.pixels.indices.map(position=>getPixel(layers,position)).toVector)

  def run(input_file:String):(Int,Int) = {
    val allPixels = readVector(input_file, sep="")
    val layers = allPixels.grouped(Layer.flatLength).map(x => Layer(x)).toList
    val minZerosLayer = layers.min(Ordering[Int].on[Layer](layer => layer.counts.getOrElse(0,0)))
    println(getStackedVector(layers).rows.mkString("\n")) //TODO as improvement, turn this into an actual image
    (minZerosLayer.counts(1)*minZerosLayer.counts(2),0)
  }

  val (result1, result2) = run(args.headOption.getOrElse("inputs/day8.csv"))
  println(s"Day8-1: $result1")
  println(s"Day8-2: $result2")
}
