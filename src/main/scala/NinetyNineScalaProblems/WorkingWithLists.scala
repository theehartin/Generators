package NinetyNineScalaProblems

import scala.annotation.tailrec

object WorkingWithLists extends App {

  def lastele[A](xs: List[A]): Any = {
    xs match{
      case Nil => "Empty List"
      case List(a) => a
      case List(_,_*) => lastele(xs.tail)
    }
  }

  def penultimate[A](xs: List[A]): Any = {
    xs match {
      case Nil => "Empty List"
      case List(a,b) => a
      case List(_,_*) => penultimate(xs.tail)
    }
  }

  def penultimateBuiltin[A](ls: List[A]): A =
    if (ls.isEmpty) throw new NoSuchElementException
    else ls.init.last


  def nth[A](k: Int, xs: List[A]): Any = {
    if (k == 0) xs.head
    else nth(k-1, xs.tail)
  }


  def myLength[A](xs:List[A]): Int = {
    @tailrec
    def myLengthRec[A](passedList: List[A], acc: Int): Int = {
      if (passedList.isEmpty) acc
      else myLengthRec(passedList.tail, acc +1)
    }
    myLengthRec(xs, 0)
  }

  def myLengthFunc[A](xs: List[A]): Int = xs.foldLeft(0)((c,_) => c+1)
  def myLengthFunc_v2(xs: List[Int]): Int = xs.fold(0)((c,b) => c+b)

  val correspondTestList1 = List(1,2,3,4)
  val correspondTestList2 = List(1,2,3,4)
  val correspondTest = correspondTestList1.corresponds(correspondTestList2)(_ == _)
  val correspondTestv2 = correspondTestList1 == correspondTestList2



  def isPalindrome(xs:List[Int]) = {
    def compare(xs: List[Int], splitindex: Int, even: Boolean): Boolean = {
      val firsthalf = xs.splitAt(splitindex)._1
      val secondhalf = if (even) xs.splitAt(splitindex)._2 else xs.splitAt(splitindex)._2.tail
      firsthalf == secondhalf.reverse
    }
    if (xs.length % 2 == 0) compare(xs, xs.length / 2, true)
    else if (xs.length % 2 != 0) compare(xs, (xs.length - 1) / 2, false)
    else false
  }


  def myReverse(xs: List[Int]) = {

  }

  def myRemoveAt[A](indexPosition: Int, xs: List[A]): List[A] ={
    if (indexPosition == 0) xs.tail
    else {
      val split = xs.splitAt(indexPosition)
      split._1 ++ split._2.tail
    }
  }

  def orderskyRemoveAt[A](indexPosition: Int, xs: List[A]): List[A] = xs.take(indexPosition) ::: xs.drop(indexPosition+1)

  val pair = ("answer", 42).getClass



  val testList = List(1,2,3,4,4,3,2,1)
  val testListv2 = List(0,1,2,3,4,5,6)
  println(orderskyRemoveAt(1,testListv2))
  println((5/2) == 2) //true
  println(pair)


}
