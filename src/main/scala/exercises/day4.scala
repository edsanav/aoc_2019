package exercises

import scala.annotation.tailrec

object day4 extends App {

  @tailrec
  def checkIncOrEqual(ls:List[Int]):Boolean = ls match {
    case x::y::_ if y < x => false
    case x::y::ls if y >= x => checkIncOrEqual(y::ls)
    case _ => true
  }

  @tailrec
  def checkRepetition(ls:List[Int]):Boolean = ls match {
    case x::y::_ if x==y => true
    case x::y::ls if x!=y => checkRepetition(y::ls)
    case _ => false
  }

  @tailrec
  def checkSingleRepetition(ls:List[Int], size:Int, previous:Int):Boolean = {
    // This is quite dirty!
    ls match {
      case x::_ if (x!=previous && size==2) =>  true
      case x::Nil if (x==previous && size==1) => true // end of list
      case x::xs if (x!=previous && size!=2) =>  checkSingleRepetition(xs, 1, x)
      case x::xs if (x==previous) => checkSingleRepetition(xs, size+1, x)
      case _ => false
    }
  }

  def toDigitsList(num:Int):List[Int] = num.toString.split("").map(_.toInt).toList


  def run(input:(Int,Int)):(Int,Int) = {
    val nums = input._1 to input._2
    val descending:List[List[Int]] = nums.map(toDigitsList).toList.filter( ls => checkIncOrEqual(ls))
    val normalReps = descending.filter(checkRepetition)
    val singleReps = normalReps.filter( ls => checkSingleRepetition(ls.tail, 1, ls.head))
    (normalReps.length, singleReps.length)
  }

  val (result1, result2) = run( (165432,707912))
  println(s"Day4-1: $result1")
  println(s"Day4-2: $result2")
}
