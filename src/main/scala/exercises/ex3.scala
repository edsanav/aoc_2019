package exercises

import cats.Foldable
import cats.syntax.traverse._  // for traverse
import exercises.algebra.Result

object ex3 {

  def toMove(inst:String):Result[(Int,Int)] = inst.toList match {
    case Nil => Left(s"Unable to process empty instruction" )
    case x::xs => {
      (x,xs.mkString.toIntOption) match {
        case ('U', Some(x)) => Right((0, x))
        case ('D', Some(x)) => Right((0,-x))
        case ('L', Some(x)) => Right((-x, 0))
        case ('R', Some(x)) => Right((x, 0))
        case _ => Left(s"Unable to process move $inst")
      }
    }
  }

  case class Point(x:Int, y:Int){
    def move(move:(Int,Int)):(Line,Point) = {
      val x2 = x+move._1
      val y2 = y+move._2
      if (x==x2) (Vertical(y,y2,x),Point(x2,y2))
      else (Horizontal(x, x2, y), Point(x2,y2))
    }
  }
  object Point{
    def apply(tupl:(Int,Int)):Point = Point(tupl._1, tupl._2)
  }

  sealed abstract class Line {
    def crossPoint(l:Line):Option[Point]
  }

  case class Horizontal(x1:Int,x2:Int,y:Int) extends Line{
    val minX:Int = math.min(x1,x2)
    val maxX:Int = math.max(x1,x2)

    override def crossPoint(l: Line): Option[Point] = l match {
      case Horizontal(_,_,_) => None
      case v:Vertical=> {
        if (v.x >= minX && v.x <= maxX && y >= v.minY && y <= v.maxY) Some(Point(v.x,y))
        else None
      }
    }
  }
  case class Vertical(y1:Int, y2:Int, x:Int) extends Line{
    val minY:Int = math.min(y1,y2)
    val maxY:Int = math.max(y1,y2)

    override def crossPoint(l: Line): Option[Point] = l match {
      case Vertical(_,_,_) => None
      case h:Horizontal => {
        if (h.y >= minY && h.y <= maxY && x >= h.minX && x <= h.maxX) Some(Point(x,h.y))
        else None
      }
    }
  }

  def moves(movesStr:List[String]):Result[List[(Int,Int)]] = movesStr.map(toMove).sequence

  def lines[F[_]: Foldable](movesStr:List[(Int,Int)], origin:(Int,Int)=(0,0)):List[Line] = {
    movesStr.foldLeft((Point(origin), List.empty[Line])){ case ( (lastPoint, lines), move) =>
      val (newLine, newPoint) = lastPoint.move(move)
      (newPoint,newLine::lines)
    }._2.reverse
  }

  def run(input:List[String]):Result[Int] = input match {
    case c1::c2::Nil => ???
    case _ => Left(s"Invalid number of cables ${input.size}")
  }

}
