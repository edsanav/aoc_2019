package exercises

object day4 extends App {

  //Quick totally dirty solution
  def checkIncOrEqual(ls:List[Int]):Boolean = ls match {
    case x::y::_ if y < x => false
    case x::y::ls if y >= x => checkIncOrEqual(y::ls)
    case _ => true
  }

  def checkRepetition(ls:List[Int]):Boolean = ls match {
    case x::y::_ if x==y => true
    case x::y::ls if x!=y => checkRepetition(y::ls)
    case _ => false
  }

  def toDigitsList(num:Int):List[Int] = num.toString().split("").map(_.toInt).toList

  def isValid(num:Int):Boolean = {
    val digits = toDigitsList(num)
    checkIncOrEqual(digits) && checkRepetition(digits)
  }

  def run(input:(Int,Int)):Int = {
    val nums = input._1 to input._2
    nums.count(isValid)
  }

  val result = run( (165432,707912))

  println(s"Day4: $result")

}
