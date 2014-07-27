package scala99

import org.scalatest.{Matchers, FlatSpec}

class AwesomeListTest extends FlatSpec with Matchers {

  val emptyList = List()
  val intList = List(1,2,3,4,5)

  "last" should "return last element" in {
    AwesomeList.lastElem(intList) shouldEqual Some(5)
  }

  it should "return nothing if list is empty" in {
    AwesomeList.lastElem(emptyList) shouldEqual None
  }

  "penultimate" should "return second last element" in {
    AwesomeList.penultimate(intList) shouldEqual Some(4)
  }

  it should "return nothing if list only has one element" in {
    AwesomeList.penultimate(List(1)) shouldEqual None
  }

  it should "return nothing if list is empty" in {
    AwesomeList.penultimate(emptyList) shouldEqual None
  }

  "kthElement" should "return kth element in the list" in {
    AwesomeList.kthElement(2, intList) shouldEqual Some(3)
  }

  it should "return nothing" in {
    AwesomeList.kthElement(-1, intList) shouldEqual None
    AwesomeList.kthElement(5, intList) shouldEqual None
    AwesomeList.kthElement(2, emptyList) shouldEqual None
  }

  "length" should "return size of the list" in {
    AwesomeList.length(intList) shouldEqual 5
    AwesomeList.length(emptyList) shouldEqual 0
  }

  "reverse" should "return reversed list" in {
    AwesomeList.reverse(intList) shouldEqual List(5,4,3,2,1)
    AwesomeList.reverse(emptyList) shouldEqual emptyList
  }

  "isPalindrome" should "return true if the list if palindrom" in {
    AwesomeList.isPalindrome(List(1,2,3,2,1)) shouldEqual true
  }

  "flatten" should "return single list of all elements" in {
    AwesomeList.flatten(List(List(1,2,3), 4, List(List(5)))) shouldEqual intList
  }

  "compress" should "return a list without duplicates in the original order" in {
    AwesomeList.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "pack" should "return consecutive duplicates of list with elements in sublists" in {
    AwesomeList.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "encode" should "return run-length encoding data compression" in {
    val original = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val encoded = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    AwesomeList.encode(original) shouldEqual encoded
    AwesomeList.decode(encoded) shouldEqual original

    AwesomeList.encodeDirect(original) shouldEqual encoded
  }

  "encodeModified" should "return run-length encoding data where no duplicates are just copied" in {
    AwesomeList.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldEqual List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  }

  "duplicate" should "duplicate the elements of a list" in {
    AwesomeList.duplicate(List('a, 'b, 'c, 'c, 'd)) shouldEqual List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }

  "dupliateN" should "duplicate the element N times" in {
    AwesomeList.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) shouldEqual List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "drop" should "drop Nth element in the list" in {
    AwesomeList.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "split" should "split a list into two parts" in {
    AwesomeList.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "slice" should "extract a slice from a list" in {
    AwesomeList.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g)
  }

  "rotate" should "rotate a list N places to the left" in {
    AwesomeList.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    AwesomeList.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldEqual List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }

  "removeAt" should "remove Nth element in a list" in {
    AwesomeList.removeAt(1, List('a, 'b, 'c, 'd)) shouldEqual (List('a, 'c, 'd),'b)
  }

  "insertAt" should "insert an element at a given position" in {
    AwesomeList.insertAt('new, 1, List('a, 'b, 'c, 'd)) shouldEqual List('a, 'new, 'b, 'c, 'd)
  }

  "range" should "create a list containing all integers within a given range" in {
    AwesomeList.range(4,9) shouldEqual List(4,5,6,7,8,9)
  }

  "randomSelect" should "extract a given number of randomly selected elements from a list" in {
    val actual = AwesomeList.randomSelect(2, List(1,2,3,4))
    actual shouldNot equal(List(1,2,3,4))
    actual.length shouldEqual 2
  }

  "lotto" should "draw N random numbers from 1..M" in {
    val actual = AwesomeList.lotto(6, 49)
    actual.length shouldEqual 6
    actual.foreach(_ should be <= 49)
    actual should not equal AwesomeList.lotto(6, 49)
  }

  "randomPermute" should "generate a random permutation of the elements of a list" in {
    val original = List(1,2,3,4,5,6)
    val actual = AwesomeList.randomPermute(original)
    actual should not equal original
    actual should not equal AwesomeList.randomPermute(original)
  }

  "combinations" should "generate the combinations of K distinct objects from a list" in {
    AwesomeList.combinations(3, List(1,2,3,4)) shouldEqual List(List(1,2,3), List(1,2,4), List(1,3,4), List(2,3,4))
  }
}
