package exercises

object day3 extends App {

  trait Move{
    val positions:Int
  }

  case class Up(positions:Int) extends Move
  case class Down(positions:Int) extends Move
  case class Left(positions:Int) extends Move
  case class Right(positions:Int) extends Move



  case class Point(x:Int, y:Int)

  case class Line(p1:Point, p2:Point){
    def apply(p1: Point, move:Move): Line =  Line(p1, p2)
  }

  def run(input_file:String):Int = ???

  val result = run(args.headOption.getOrElse("inputs/day3.csv"))

  println(s"Day3: $result")
}
