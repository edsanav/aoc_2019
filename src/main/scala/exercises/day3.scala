package exercises

import scala.io.Source
import Math.{max, min}
object day3 extends App {

  type Cable = List[Line]

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

  case class Point(x:Int, y:Int){
    def move(m:Move):Line = m match {
      case m:Left => Horizontal(y=this.y, x1=this.x, x2=this.x-m.positions)
      case m:Right => Horizontal(y=this.y, x1=this.x, x2=this.x+m.positions)
      case m:Up => Vertical(x=this.x, y1=this.y, y2=this.y+m.positions)
      case m:Down => Vertical(x=this.x, y1=this.y, y2=this.y-m.positions)
    }

    lazy val distance:Int = Math.abs(x)+Math.abs(y)
  }

  object Line{
    def unapply(l: Line): Option[(Point, Point)] = Some(l.p1, l.p2)
  }

  sealed trait Line{
    val p1:Point
    val p2:Point

    def crossPoint(l:Line):Option[Point]
  }

  case class Horizontal(p1:Point, p2:Point) extends Line{
    val y:Int = p1.y
    val x1:Int = p1.x
    val x2:Int = p2.x

    lazy val minX:Int = min(x1,x2)
    lazy val maxX:Int = max(x1,x2)


    def crossPoint(l:Line):Option[Point] = l match {
      case l:Vertical if (minX <= l.x  &&  l.x <= maxX) && (l.minY <= y  &&  y <= l.maxY) => Some(Point(l.x, y))
      case _ => None
    }

  }
  // Alternate constructor
  object Horizontal{
    def apply(y:Int, x1:Int, x2:Int):Horizontal = Horizontal(Point(x1,y), Point(x2, y))
  }

  case class Vertical(p1:Point, p2:Point) extends  Line {
    val x: Int = p1.x
    val y1: Int = p1.y
    val y2: Int = p2.y

    lazy val minY:Int = min(y1,y2)
    lazy val maxY:Int = max(y1,y2)

    def crossPoint(l:Line):Option[Point] = l match {
      case l:Horizontal if (minY <= l.y  &&  l.y <= maxY) && (l.minX <= x  &&  x <= l.maxX) => Some(Point(x, l.y))
      case _ => None
    }
  }

  object Vertical{
    def apply(x:Int, y1:Int, y2:Int):Vertical = Vertical(Point(x,y1),Point(x,y2))
  }

  def readMoves(input_file:String):List[List[Move]] = {
    Source.fromResource(input_file).getLines.map(s => s.split(',').toList.map(Move.fromString)).toList
  }

  def movesToCable(moves:List[Move], start:Point=Point(0,0)):Cable = {
    moves.tail.scanLeft(start.move(moves.head))((l:Line, m:Move) => l.p2.move(m))
  }

  def crossPoints(c1:Cable, c2:Cable):List[Point] = {
    val combinations = for {
      l1 <- c1
      l2 <- c2
    } yield l1.crossPoint(l2)
    combinations.flatten.filter(_!=Point(0,0))
  }

  def closestPoint(l:List[Point]):Point = {
    l.tail.foldLeft(l.head)((closest,point) => if (point.distance < closest.distance) point else closest)
  }


  def run(input_file:String):Int = {
    val cables:List[Cable] = readMoves(input_file).map(movesToCable(_))
    val cableA:Cable = cables.head
    val cableB:Cable = cables.tail.head
    closestPoint(crossPoints(cableA, cableB)).distance
  }

  val result = run(args.headOption.getOrElse("inputs/day3.csv"))
  println( println(s"Day3: $result")
  )

}
