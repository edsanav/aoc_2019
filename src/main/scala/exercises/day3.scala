package exercises

object day3 extends App {

  trait Move{
    val positions:Int
  }
  object Move {
    def fromString(s:String):Move =  {
      val positions:Int = s.drop(1).toInt
      s.charAt(0) match {
        case 'U' => Up(positions)
        case 'D' => Down(positions)
        case 'R' => Right(positions)
        case 'L' => Left(positions)
      }
    }
  }

  trait VerticalMove extends Move
  trait HorizontalMove extends Move

  case class Up(positions:Int) extends VerticalMove
  case class Down(positions:Int) extends VerticalMove
  case class Left(positions:Int) extends HorizontalMove
  case class Right(positions:Int) extends HorizontalMove

  case class Point(x:Int, y:Int)

  object Line{
    def unapply(l: Line): Option[(Point, Point)] = Some(l.p1, l.p2)
  }

  sealed trait Line{
    val p1:Point
    val p2:Point
  }

  case class Horizontal(y:Int, x1:Int, x2:Int) extends Line{
    def apply(p:Point, m:HorizontalMove): Line = m match {
      case m:Left => Horizontal(y=p.y, x1=p.x, x2=p.x-m.positions)
      case m:Right => Horizontal(y=p.y, x1=p.x, x2=p.x+m.positions)
    }
    val p1=Point(x=x1, y=y)
    val p2=Point(x=x2, y=y)

    def crossPoint(l:Line):Option[Point] = l match {
      case l:Vertical if (x1 <= l.x  &&  l.x <= x2) =>Some(Point(l.x, y))
      case _ => None
    }

  }

  case class Vertical(x:Int, y1:Int, y2:Int) extends  Line {

    def apply(p:Point, m:VerticalMove): Line = m match {
      case m:Up => Vertical(x=p.x, y1=p.y, y2=p.y+m.positions)
      case m:Down => Vertical(x=p.x, y1=p.y, y2=p.y-m.positions)
    }
    val p1=Point(x=x, y=y1)
    val p2=Point(x=x, y=y2)

    def crossPoint(l:Line):Option[Point] = l match {
      case l:Horizontal if (y1 <= l.y  &&  l.y <= y2) => Some(Point(x, l.y))
      case _ => None
    }

  }

  def run(input_file:String):Int = ???

  val result = run(args.headOption.getOrElse("inputs/day3.csv"))

  println(s"Day3: $result")
}
