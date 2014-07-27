package scala99

import scala.collection.immutable.IndexedSeq
import scala.util.{Random, Success, Failure, Try}

object AwesomeList {
  /**
   *  P01 (*) Find the last element of a list.
   */
  def lastElem[A](list: List[A]) = Try(list.reverse.head) match {
    case Success(last) => Some(last)
    case Failure(e) => None
  }

  /**
   * P02 (*) Find the last but one element of a list.
   */
  def penultimate[A](list: List[A]): Option[A] = list match {
    case head :: tail :: Nil => Some(head)
    case _ :: tail => penultimate(tail)
    case _ => None
  }

  /**
   * P03 (*) Find the Kth element of a list.
   */
  def kthElement[A](k: Int, list: List[A]): Option[A] = (k, list) match {
    case (0, h :: _) => Some(h)
    case (k, _ :: tail) => kthElement(k-1, tail)
    case _ => None
  }

  /**
   * P04 (*) Find the number of elements of a list.
   */
  def length[A](list: List[A]) = list.foldLeft(0)((acc, a) => acc + 1)

  /**
   * P05 (*) Reverse a list.
   */
  def reverse[A](list: List[A]) = list.foldLeft(List[A]())( (b, a) => a::b )

  /**
   * P06 (*) Find out whether a list is a palindrome.
   */
  def isPalindrome[A](list: List[A]): Boolean = list match {
    case head :: Nil => true
    case head :: tail :: Nil => head == tail
    case head :: tail => if (head == tail.last) isPalindrome(tail.takeRight(1)) else false
  }

  /**
   * P07 (**) Flatten a nested list structure.
   */
  def flatten(list: List[Any]): List[Any] = {
    list.flatMap(elem => elem match {
      case elem: List[_] => flatten(elem)
      case elem => List(elem)
    })
  }

  /**
   * P08 (**) Eliminate consecutive duplicates of list elements.
   */
  def compress[A](list: List[A]) = {
    list.foldLeft(List(list.head))((acc, elem)=> if (acc.last != elem) acc :+ elem else acc)
  }

  /**
   * P09 (**) Pack consecutive duplicates of list elements into sublists.
   */
  def pack[A](list: List[A]): List[List[A]] = {
    if (list.isEmpty) List(List())
    else {
      val (packed, next) = list span {_ == list.head}
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  /**
   * P10 (*) Run-length encoding of a list.
   */
  def encode[A](list: List[A]) = {
    pack(list) map (l => (l.size, l.head))
  }

  /**
   * P11 (*) Modified run-length encoding.
   */
  def encodeModified[A](list: List[A]) = {
    pack(list) map {
      case l if l.size > 1 => (l.size, l.head)
      case l => l.head
    }
  }

  /**
   * P12 (**) Decode a run-length encoded list.
   */
  def decode[A](list: List[(Int, A)]) = list flatMap {
    case (s,l) => for (i <- 1 to s) yield l
  }

  /**
   * P13 (**) Run-length encoding of a list (direct solution).
   */
  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    if (list.isEmpty) Nil
    else {
      val (packed, next) = list span {_ == list.head}
      (packed.size, packed.head) :: encodeDirect(next)
    }
  }

  /**
   * P14 (*) Duplicate the elements of a list.
   */
  def duplicate[A](list: List[A]): List[A] = list match {
    case head :: Nil => head :: head :: Nil
    case head :: tail => head :: head :: duplicate(tail)
  }

  /**
   * P15 (**) Duplicate the elements of a list a given number of times.
   */
  def duplicateN[A](n: Int, list: List[A]) = list flatMap {
    e => for (_ <- 1 to n) yield e
    //e => List.tabulate(n)(_ => e) // This also works
  }

  /**
   * P16 (**) Drop every Nth element from a list.
   */
  def drop[A](n: Int, list: List[A]) = {
    list.foldLeft((1, List[A]())){
      (acc, e) =>
        if (acc._1 % 3 > 0) (acc._1 + 1, acc._2 :+ e)
        else (acc._1 + 1, acc._2)
    }._2
  }

  /**
   * P17 (*) Split a list into two parts.
   */
  def split[A](n: Int, list: List[A]) = {
    (list.take(n), list.drop(n))
  }

  /**
   * P18 (**) Extract a slice from a list.
   */
  def slice[A](from: Int, to: Int, list: List[A]) = {
    list.drop(from).dropRight(list.length-to)
  }

  /**
   * P19 (**) Rotate a list N places to the left.
   */
  def rotate[A](n: Int, list: List[A]): List[A] = {
    if (n == 0) list
    else if (n > 0)
      list.drop(n) ++ list.take(n)
    else
      list.takeRight(-n) ++ list.dropRight(-n)
  }

  /**
   * P20 (*) Remove the Kth element from a list.
   */
  def removeAt[A](n: Int, list: List[A]) = {
    (list.take(n+1).init ++ list.drop(n+1), list.take(n+1).last)
  }

  /**
   * P21 (*) Insert an element at a given position into a list.
   */
  def insertAt[A](e: A, n: Int, list: List[A]) = {
    (list.take(n) :+ e) ++ list.drop(n)
  }

  /**
   * P22 (*) Create a list containing all integers within a given range.
   */
  def range(start: Int, end: Int) = {
    (for (i <- (start to end)) yield List(i)).flatten
  }

  /**
   * P23 (**) Extract a given number of randomly selected elements from a list.
   */
  def randomSelect[A](n: Int, list: List[A]): List[A] = {
    Random.shuffle(list).take(n)
  }

  /**
   * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
   */
  def lotto(n: Int, m: Int): List[Int] = {
    AwesomeList.randomSelect(n, (1 to m).toList)
  }

  /**
   * P25 (*) Generate a random permutation of the elements of a list.
   */
  def randomPermute[A](list: List[A]) = {
    randomSelect(list.length, list)
  }

  /**
   * P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
   */
  def combinations[A](n: Int, list: List[A]): List[List[A]] = ???
}