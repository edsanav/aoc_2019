package exercises

import cats.{Foldable, Semigroupal}
import cats.instances.either._
import cats.syntax.parallel._
import cats.syntax.traverse._

import exercises.algebra.Result

// TODO extend list functionalities
object ex3 {

  def toMove(inst: String): Result[(Int, Int)] = inst.toList match {
    case Nil => Left(s"Unable to process empty instruction")
    case x :: xs => {
      (x, xs.mkString.toIntOption) match {
        case ('U', Some(x)) => Right((0, x))
        case ('D', Some(x)) => Right((0, -x))
        case ('L', Some(x)) => Right((-x, 0))
        case ('R', Some(x)) => Right((x, 0))
        case _ => Left(s"Unable to process move $inst")
      }
    }
  }

  case class Point(x: Int, y: Int) {
    def move(move: (Int, Int)): (Line, Point) = {
      val x2 = x + move._1
      val y2 = y + move._2
      if (x == x2) (Vertical(y, y2, x), Point(x2, y2))
      else (Horizontal(x, x2, y), Point(x2, y2))
    }

    def mDistance: Int = x.abs + y.abs
  }

  object Point {
    def apply(tupl: (Int, Int)): Point = Point(tupl._1, tupl._2)
  }

  sealed abstract class Line {
    def crossPoint(l: Line): Option[Point]

    def movedAndReached(p: Point): (Int, Boolean)
  }

  case class Horizontal(x1: Int, x2: Int, y: Int) extends Line {
    val minX: Int = math.min(x1, x2)
    val maxX: Int = math.max(x1, x2)

    override def crossPoint(l: Line): Option[Point] = l match {
      case Horizontal(_, _, _) => None
      case v: Vertical => {
        if (v.x >= minX && v.x <= maxX && y >= v.minY && y <= v.maxY) Some(Point(v.x, y))
        else None
      }
    }

    override def movedAndReached(p: Point): (Int, Boolean) =
      if (p.y == y && (minX <= p.x || p.x <= maxX)) ((p.x - x1).abs, true)
      else ((x2 - x1).abs, false)
  }

  case class Vertical(y1: Int, y2: Int, x: Int) extends Line {
    val minY: Int = math.min(y1, y2)
    val maxY: Int = math.max(y1, y2)

    override def crossPoint(l: Line): Option[Point] = l match {
      case Vertical(_, _, _) => None
      case h: Horizontal => {
        if (h.y >= minY && h.y <= maxY && x >= h.minX && x <= h.maxX) Some(Point(x, h.y))
        else None
      }
    }

    override def movedAndReached(p: Point): (Int, Boolean) =
      if (p.x == x && (minY <= p.y || p.y <= maxY)) ((p.y - y1), true)
      else ((y2 - y1).abs, false)
  }

  case class PointTracker(notFound: Map[Point, Int], reached: Map[Point, Int]) {
    //TODO clean this a bit
    def process(l: Line): PointTracker = {
      notFound.foldLeft(PointTracker(Map.empty[Point, Int], reached)) { case (pt, (p, accum)) =>
        val (moves, isReached) = l.movedAndReached(p)
        if (isReached) PointTracker(pt.notFound, pt.reached + (p -> (accum + moves)))
        else PointTracker(pt.notFound + (p -> (accum + moves)), pt.reached)
      }
    }
  }

  def trackPoints(cable: List[Line], points: List[Point]): PointTracker = {
    val initialPointTracker = PointTracker(points.map((_, 0)).toMap, Map.empty[Point, Int])
    cable.foldLeft(initialPointTracker) { case (tracker, line) => tracker.process(line) }
  }


  def toMoves(movesStr: String): Result[List[(Int, Int)]] = movesStr.split(",").map(toMove).toList.sequence

  //TODO extend implicits
  def toLines[F[_] : Foldable](moves: List[(Int, Int)], origin: (Int, Int) = (0, 0)): List[Line] = {
    moves.foldLeft((Point(origin), List.empty[Line])) { case ((lastPoint, lines), move) =>
      val (newLine, newPoint) = lastPoint.move(move)
      (newPoint, newLine :: lines)
    }._2.reverse
  }

  def crosses(cable1: List[Line], cable2: List[Line]): List[Point] =
    Semigroupal[List].product(cable1, cable2).flatMap { case (l1, l2) => l1.crossPoint(l2) }.filter {
      case Point(0, 0) => false
      case _ => true
    }

  //TODO split this
  def run(input: List[String]): Result[Int] = input match {
    case cs1 :: cs2 :: Nil => (
      toMoves(cs1).map(toLines(_)),
      toMoves(cs2).map(toLines(_)))
      .parMapN { case (l1, l2) => crosses(l1, l2) }
      .flatMap {
        case x :: xs => Right(xs.foldLeft(x.mDistance) { case (z, p) => if (p.mDistance < z) p.mDistance else z })
        case Nil => Left("Not cross points found")
      }
    case _ => Left(s"Invalid number of cables ${input.size}")
  }

}
