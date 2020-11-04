package exercises

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

  case class Point(x:Int, y:Int)
  object Point{
    def apply(tupl:(Int,Int)):Point = Point(tupl._1, tupl._2)
  }

  sealed abstract case class Line(origin:Point, end:Point){
    def crossPoint(l:Line):Option[Point]
  }
  case class Horizontal(x1:Int,x2:Int,y:Int) extends Line(Point(x1,y), Point(x2,y)){
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
  case class Vertical(y1:Int, y2:Int, x:Int) extends Line(Point(x,y1), Point(x,y2)){
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



  def run(input:List[String]):Result[Int] = ???

}
